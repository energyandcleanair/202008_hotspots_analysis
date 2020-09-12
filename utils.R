utils.save_config <- function(dir_results, formula, interval, cutoff_date){
  sink(file.path(dir_results,"config.txt"))
  print(formula)
  print(paste("Interval:", interval))
  print(paste("Cutoff date:", cutoff_date))
  sink()
}

utils.read_points <- function(){

  p <- readxl::read_xlsx(file.path("data", "OMI_Catalogue_Emissions_2005-2019_ILS-update.xlsx"),
                        range="A3:BQ591") %>%
    dplyr::select(-c(SOURCETY)) %>%
    dplyr::rename(SOURCETY=`New Sourcety`)

  s <- sf::st_as_sf(p, coords=c("LONGITUDE", "LATITUDE"), crs="+proj=longlat +datum=WGS84") #Same crs as raster to avoid transformations

  return(s)
}

utils.buffer <- function(radius_km, points){

  if(is.null(radius_km) | radius_km==0) return(points %>% dplyr::mutate(radius_km=radius_km))

  crs_org = sf::st_crs(points)
  # Convert to EPSG:3857 to have a meter unit
  sf::st_transform(points, crs=3857) %>%
    sf::st_buffer(radius_km*1000) %>%
    sf::st_transform(crs=crs_org) %>%
    dplyr::mutate(radius_km=radius_km)
}

utils.date_from_filename <- function(filename){
  ymd <- sub(".*OMSO2e_([0-9]{4})m([0-9]{4}).*","\\1\\2", filename, perl=T)
  return (as.POSIXct(ymd, format="%Y%m%d", tz="UTC"))
}


utils.values_at_point <- function(file, points){
  tryCatch({
    r <- raster::brick(file, varname="ColumnAmountSO2_PBL")
    # values <- raster::extract(r, points)[,1]
    points <- rgis::fast_extract(
      sf = points,
      ras = r,
      funct = "mean.na",
      small.algo = T, #important
      col.names = NULL,
      parallel = TRUE,
      n.cores = NULL
    ) %>% rename(value=r_layer)
    points$date <- utils.date_from_filename(file)
    points_lite <- tibble(points) %>% dplyr::select(-c(geometry))
    return(points_lite)

  }, error=function(e){
    print(paste("Failed for file",file))
    return(NA)
  })
}


utils.add_predicted_and_ci <- function(d.omi.pred, m, interval, lm_per_source){

  # We add predicted values and its confidence interval to d.omi.pred
  # Note: the confidence interval stems from both uncertainties:
  # NASA MEaSUREs ("sigma_original") and correlation ("sigma_lm")
  # https://math.stackexchange.com/questions/1387586/how-to-propagate-uncertainties-in-the-dependent-variable-when-doing-linear-regre

  predict_and_ci <- function(m, d, interval){
    # IMPORTANT: we use interval='confidence' but it could well be interval='prediction',
    # in which case the confidence interval (i.e. prediction interval in that instance)
    # is significantly wider
    predicted <- predict(m, d, interval=interval, se.fit=T, level=0.95)

    # of the expected value, hence 'confidence' (vs 'prediction')
    d$predicted <- pmax(predicted$fit[,'fit'], 0)
    # d$sigma_lm = predicted$se.fit #se.fit not responsive to interval
    d$sigma_lm <- (predicted$fit[,'upr']-predicted$fit[,'lwr'])/2/1.96

    # We assume the NASA error and the current model residuals are independent
    # Variance equivalent = variance due to lm + variance due to NASA uncertainty
    d %>% mutate(predicted_upr=predicted + 1.96*sqrt(sigma_lm^2 + sigma_original^2),
                 predicted_lwr=predicted - 1.96*sqrt(sigma_lm^2 + sigma_original^2))
  }

  if(lm_per_source){
    d.omi.pred %>%
      left_join(m) %>%
      ungroup() %>%
      nest_by(NUMBER, year_offsetted, m) %>%
      mutate(predicted=list(predict_and_ci(m, data, interval) %>%
                              dplyr::select(predicted, predicted_upr, predicted_lwr, sigma_lm))) %>%
      unnest(predicted)  %>%
      unnest(data) %>%
      ungroup() %>%
      dplyr::select(-c(m))
  }else{
    predict_and_ci(m, d.omi.pred, interval)
  }
}

utils.CI.Fieller = function(theta1.hat, sd1.hat, year_1,
                            theta2.hat, sd2.hat, year_2,
                            alpha=0.05 # theoretical coverage (1-alpha)
)
{
  if(year_2!=year_1-1 | is.na(year_1+year_2)){
    return(NA)
  }
  ## CI Fieller
  # Raftery, A E, and T. Schweder. 1993.
  ## “Inference About the Ratio of Two Parameters, with Application to
  # Whale Censusing.” The American Statistician 47 (4): 259–64. Eq. 1-2
  # We assume here independence between theta1.hat and theta2.hat
  # the size of the sample used to obtain the estimates
  # and their sd is assumed to be unknown
  # we substitute the quantile of a Normal(0,1) to that of a St(?)
  # that should be used in principle
  z = qnorm(p=1-alpha/2)
  if( (theta1.hat/sd1.hat <= z) & (theta2.hat/sd2.hat <= z) )
  {
    L = 0 ; U = Inf
  }else
  {
    rad = (theta1.hat^2 * sd2.hat^2 + theta2.hat^2 * sd1.hat^2 -
             z^2 * sd1.hat^2 * sd2.hat^2 )
    L = (theta1.hat * theta2.hat - z*sqrt(rad) )/( theta2.hat^2 - z * sd2.hat^2 )
    U = (theta1.hat * theta2.hat + z*sqrt(rad) )/( theta2.hat^2 - z * sd2.hat^2 )
    if((theta1.hat/sd1.hat > z) & (theta2.hat/sd2.hat <= z) ){L = U ; U = Inf}
    if((theta1.hat/sd1.hat <= z) & (theta2.hat/sd2.hat > z) ){L = 0}
  }
  R.hat = theta1.hat/theta2.hat
  res = tibble(R.hat,L,U)
  # names(res) = c('R.hat','L','U')
  return(res)

}
## END CI Fieller
#
# utils.add_yoy_with_ci <- function(d.omi.pred, group_by_cols=c("SOURCETY")){
#
#   d.omi.pred %>%
#     filter(!is.na(SOURCETY), year_offsetted>=2000) %>%
#     group_by_at(c(group_by_cols, "year_offsetted")) %>%
#     summarise(value=sum(predicted, na.rm=T),
#               lwr=sum(predicted_lwr, na.rm=T),
#               upr=sum(predicted_upr, na.rm=T),
#               sigma_eq_lm=sqrt()) %>%
#     group_by_at(group_by_cols) %>%
#     arrange(year_offsetted) %>%
#     mutate(ratio_yoy_dfr==pmap(list(lag(value), lag(), year_1,
#                                     theta2.hat, sd2.hat, year_2)ifelse(lag(year_offsetted)==year_offsetted-1,
#                             (value-lag(value))/lag(value),
#                             NA),
#            ratio_yoy_lwr=ifelse(lag(year_offsetted)==year_offsetted-1,
#                                 (lwr-lag(value))/lag(value),
#                                 NA),
#            ratio_yoy_upr=ifelse(lag(year_offsetted)==year_offsetted-1,
#                                 (upr-lag(value))/lag(value),
#                                 NA))
#
#     r <- utils.CI.Fieller(value_before, sigma_before, value_after, sigma_after, 0.05)
#
# }

utils.prediction_ytd <- function(d.all.year, d.omi, date_to_year, formula, interval, dir_results, lm_per_source){

  # We train models on d.all.year for every calendar year (the only periods we have Measures data for)
  # And then apply to different year cutting (e.g. August to July)
  d <- d.all.year %>% mutate(NUMBER=as.integer(NUMBER), year=as.integer(year))
  d  %<>% filter(SOURCETY != "Volcano")
  d  %<>% distinct(NUMBER, year, value_original) %>%
    group_by(NUMBER) %>%
    summarise(value_original_mean = mean(value_original, na.rm=T)) %>%
    full_join(d, .)

  dw <- d %>%
    mutate(radius_km = paste0('r', radius_km)) %>%
    dplyr::select(-count_omi) %>%
    spread(radius_km, value_omi) %>%
    mutate(variation=value_original-value_original_mean) %>%
    ungroup()

  if(!lm_per_source){
    lm(formula, data=dw) -> m

    sink(file.path(dir_results, "m_summary.txt"))
    print(summary(m))
    sink()

    # Plotting all correlations
    dw_plot <- dw %>% mutate(predicted = predict(m, dw))
  }else{
    # One lm per source
    # m is a tibble of ms
    m <- dw %>%
           group_by(NUMBER) %>%
           do(m=lm(value_original ~ (r10 + r50 + r100 + r200), .)) %>%
           mutate(
             r.squared=summary(m)$r.squared,
             standard_error=summary(m)$sigma
           )

    sink(file.path(dir_results, "m_summary.txt"))
    print(m)
    print(summary(m %>% dplyr::select(r.squared, standard_error)))
    sink()

    dw_plot <- m %>% mutate(data=list(m$model),
                 predicted=list(m$fitted.values)) %>%
      unnest(cols=c(data,predicted))

  }

  # Plotting all correlations
  p <- plot_all_correlations(dw_plot)
  ggsave(file.path(dir_results, "all_correlations.png"), plot=p, width=36, height=36)

  # We average NASA sigma based on cutoff date
  days_of_year <- function(year){
    tibble(seq(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-31")), by="+1 day"))
  }

  d.meta <- d  %>%
    ungroup() %>%
    distinct(NUMBER, SOURCETY, COUNTRY, ELEVATION, year, value_original_mean, sigma_original) %>%
    rowwise() %>%
    mutate(date=as.list(purrr::map_dfr(year, days_of_year))) %>%
    tidyr::unnest(cols=c(date)) %>%
    mutate(year_offsetted=date_to_year(date)) %>%
    group_by(NUMBER, SOURCETY, COUNTRY, ELEVATION, value_original_mean, year_offsetted) %>%
    summarise(sigma_original=mean(sigma_original, na.rm=T))

  d.omi.pred <- d.omi %>%
    mutate(year_offsetted= date_to_year(date)) %>%
    filter(!is.na(year_offsetted),
           year_offsetted > lubridate::year(min(date)),
           year_offsetted < 2021) %>%
    group_by(NUMBER, year_offsetted, radius_km) %>%
    summarise(value_omi=mean(value, na.rm=T)) %>%
    mutate(radius_km = paste0('r', radius_km)) %>%
    spread(radius_km, value_omi) %>%
    right_join(d.meta)

  d.omi.pred %<>% utils.add_predicted_and_ci(m, interval, lm_per_source)

  # Add y-o-y ratio with CI
  ratios <- utils.table_yoy(d.omi.pred, group_by_cols = c("NUMBER")) %>%
    dplyr::select(NUMBER, year_offsetted, diff_yoy, diff_yoy_lwr, diff_yoy_upr)

  d.omi.pred <- d.omi.pred %>%
    merge(ratios, by=c("NUMBER", "year_offsetted"))

  # d.omi.pred$diff_yoy[is.infinite(d.omi.pred$diff_yoy)] <- 100


  return(list("d.omi.pred"=d.omi.pred, "m"=m))
}

utils.table_total <- function(d.omi.pred, group_by_cols=c("SOURCETY")){
  d.omi.pred %>%
    mutate(variance=sigma_lm^2 + sigma_original^2) %>%
    filter(!is.na(SOURCETY)) %>%
    group_by_at(c(group_by_cols, "year_offsetted")) %>%
    summarise(value=sum(predicted, na.rm=T),
              variance=sum(variance, na.rm=T)) %>%
    mutate(upr=value + 1.96*sqrt(variance),
           lwr=value - 1.96*sqrt(variance))
}

utils.table_yoy <- function(d.omi.pred, group_by_cols=c("SOURCETY")){

  d <- d.omi.pred

  dyoy <-
    d %>%
    filter(!is.na(SOURCETY)) %>%
    filter(year_offsetted>=2007) %>% # First year creates troubles
    mutate(variance=sigma_lm^2 + sigma_original^2) %>%
    group_by_at(c(group_by_cols, "year_offsetted")) %>%
    summarise(predicted=sum(predicted, na.rm=T),
              variance=sum(variance, na.rm=T)) %>%


    group_by_at(group_by_cols) %>%
    arrange(year_offsetted) %>%
    mutate(year_offsetted.lag = lag(year_offsetted,1),
           variance.lag = lag(variance,1),
           predicted.lag = lag(predicted,1)
    ) %>%
    rowwise() %>%
    filter(year_offsetted.lag==year_offsetted-1,
           !is.na(predicted.lag),
           !is.na(predicted)) %>%
    mutate(test=list(utils.CI.Fieller(theta1.hat=predicted,
                                      sd1.hat=sqrt(variance),
                                      year_1=year_offsetted,
                                      theta2.hat=predicted.lag,
                                      sd2.hat=sqrt(variance.lag),
                                      year_2=year_offsetted.lag))) %>%
    tidyr::unnest(test) %>%
    mutate(diff_yoy=R.hat-1,
           diff_yoy_lwr=L-1,
           diff_yoy_upr=U-1,
           year_offsetted_text = paste(year_offsetted,"/",year_offsetted.lag)
    ) %>%
    dplyr::select(-c(year_offsetted.lag,
                     variance.lag,
                     predicted.lag,
                     R.hat,
                     L,
                     U))

  dyoy %>%
    # filter(year_offsetted==2020) %>%
    dplyr::select_at(c(group_by_cols, "year_offsetted", "diff_yoy", "diff_yoy_lwr", "diff_yoy_upr")) %>%
    mutate(diff_yoy_str=paste0(round(diff_yoy*100,1), "%"),
           diff_yoy_lwr_str=paste0(round(diff_yoy_lwr*100,1), "%"),
           diff_yoy_upr_str=paste0(round(diff_yoy_upr*100,1), "%"))
}


utils.table_yoy_w_europe <- function(d.omi.pred, group_by_cols=c("COUNTRY")){

  selected <- c("India", "Russia", "China", "Mexico", "South Africa", "Saudi Arabia", "Europe", "Australia")

  europe <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", "Czech Republic",
              "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
              "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
              "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",
              # Below are "European countries" not in the EU
              "Ukraine", "UK", "Serbia", "Albania")

  d <- d.omi.pred
  d$COUNTRY[d$COUNTRY %in% europe] <- "Europe"
  d <- d %>% filter(COUNTRY %in% selected)

  return(utils.table_yoy(d, group_by_cols))
}
