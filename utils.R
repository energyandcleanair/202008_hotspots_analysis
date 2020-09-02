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


utils.add_predicted_and_ci <- function(d.omi.pred, m){

  predicted <- predict(m, d.omi.pred, interval = 'confidence') #We're looking at the confidence interval
  # of the expected value, hence 'confidence' (vs 'prediction')
  d.omi.pred$predicted <- pmax(predicted[,'fit'], 0)
  d.omi.pred$sigma_eq_lm = (predicted[,'upr']-predicted[,'lwr'])/2/1.96

  # We assume the NASA error and the current model residuals are independent
  # Variance equivalent = variance due to lm + variance due to NASA uncertainty
  d.omi.pred %>% mutate(predicted_upr=predicted + 1.96*sqrt(sigma_eq_lm^2 + sigma_original^2),
                        predicted_lwr=predicted - 1.96*sqrt(sigma_eq_lm^2 + sigma_original^2))

}

utils.CI.Fieller = function(theta1.hat, sd1.hat, year_1,
                            theta2.hat, sd2.hat, year_2,
                            alpha=0.05 # theoretical coverage (1-alpha)
)
{
  if(year_2!=year_1+1){
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
  res = c(R.hat,L,U)
  names(res) = c('R.hat','L','U')
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

utils.prediction_ytd <- function(d.all.year, d.omi, date_to_year){

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
    mutate(variation=value_original-value_original_mean)

  lm(value_original ~ value_original_mean + (r50 + r200):(SOURCETY + ELEVATION), data=dw) -> m
  summary(m)

  # We average sigma based on cutoff date
  days <- function(year_start, year_end){
    seq(as.Date(paste0(year_start,"-01-01")), as.Date(paste0(year_end,"-12-31")), by="+1 day")
  }

  d.meta <- d %>%
    ungroup() %>%
    distinct(NUMBER, SOURCETY, COUNTRY, ELEVATION, value_original_mean, sigma_original) %>%
    tidyr::crossing(date=days(2005,2020)) %>%
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
    left_join(d.meta)

  d.omi.pred %<>% utils.add_predicted_and_ci(m)
  d.omi.pred <- d.omi.pred %>%
    group_by(NUMBER) %>%
    arrange(year_offsetted) %>%
    mutate(ratio_yoy=ifelse(lag(year_offsetted)==year_offsetted-1,
                            (predicted-lag(predicted))/lag(predicted),
                            NA)) %>%
    mutate(year_offsetted_text = paste(year_offsetted,"/",year_offsetted-1))

  d.omi.pred$ratio_yoy[is.infinite(d.omi.pred$ratio_yoy)] <- 100

  # Other attempts or baselines
  # summary(lm(value_original ~ value_original_mean + (r50 + r200)*(SOURCETY + ELEVATION), data=dw))
  # summary(lm(value_original ~ value_original_mean, data=dw))
  # summary(lm(variation ~ (r50 + r200):(SOURCETY + ELEVATION), data=dw))
  # summary(lm(variation ~ factor(NUMBER) + (r50 + r200):(SOURCETY + ELEVATION), data=dw))
  # summary(lm(value_original ~ (r50 + r200):(SOURCETY + ELEVATION), data=dw))
  # summary(lm(value_original ~ (r10 + r20 + r50 + r100 + r200):(SOURCETY + ELEVATION), data=dw))
  # predict(m, dw) -> dw$value_original_pred

  # Plot some performance or simple correlation charts
  # plt_all <- dw %>%
  #   ggplot(aes(value_original, value_original_pred, color=SOURCETY)) +
  #   geom_point() +
  #   geom_smooth(method = "lm") +
  #   facet_wrap(~NUMBER, scales='free')
  #
  # ggsave(file.path("results","plots","pred_vs_observed.png"), plot=plt_all, width=28, height=26, scale=1.3)
  #
  #
  # plt_pearson <- dw %>%
  #   group_by(NUMBER, SOURCETY) %>%
  #   summarise(pearson=cor(value_original, value_original_pred, method = c("pearson"))) %>%
  #   ggplot() +
  #   geom_boxplot(aes(SOURCETY, pearson)) +
  #   theme_crea() +
  #   labs(subtitle="Correlation between predicted and original SO2 emissions",
  #             y="Pearson's r",
  #             x=NULL)
  # ggsave(file.path("results","plots","pearson_sourcetype.png"), plot=plt_pearson, width=12, height=10)


  return(d.omi.pred)
}

utils.table_yoy <- function(d.omi.pred, group_by_cols=c("SOURCETY")){
  d <- d.omi.pred

  dyoy <- d %>%
    group_by_at(c(group_by_cols, "year_offsetted")) %>%
    summarise(predicted=sum(predicted, na.rm=T)) %>%
    group_by_at(group_by_cols) %>%
    arrange(year_offsetted) %>%
    mutate(ratio_yoy=ifelse(lag(year_offsetted)==year_offsetted-1,
                            (predicted-lag(predicted))/lag(predicted),
                            NA)) %>%
    mutate(year_offsetted_text = paste(year_offsetted,"/",year_offsetted-1))

  dyoy %>%
    filter(year_offsetted==2020) %>%
    dplyr::select_at(c(group_by_cols, "year_offsetted", "ratio_yoy")) %>%
    mutate(ratio_yoy=paste0(round(ratio_yoy*100,1), "%"))
}


utils.country_table_yoy <- function(d.omi.pred, group_by_cols=c("COUNTRY")){

  selected <- c("India", "Russia", "China", "Mexico", "South Africa", "Saudi Arabia", "Europe", "Australia")
  d <- d.omi.pred

  europe <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", "Czech Republic",
              "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
              "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
              "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",
              # Below are "European countries" not in the EU
              "Ukraine", "UK", "Serbia", "Albania")

  d$COUNTRY[d$COUNTRY %in% europe] <- "Europe"

  dyoy <- d %>%
    group_by_at(c(group_by_cols, "year_offsetted")) %>%
    summarise(predicted=sum(predicted, na.rm=T)) %>%
    group_by_at(group_by_cols) %>%
    arrange(year_offsetted) %>%
    mutate(ratio_yoy=ifelse(lag(year_offsetted)==year_offsetted-1,
                            (predicted-lag(predicted))/lag(predicted),
                            NA)) %>%
    mutate(year_offsetted_text = paste(year_offsetted,"/",year_offsetted-1))

  dyoy %>%
    filter(COUNTRY %in% selected,
           year_offsetted==2020) %>%
    dplyr::select_at(c(group_by_cols, "year_offsetted", "ratio_yoy")) %>%
    mutate(ratio_yoy=paste0(round(ratio_yoy*100,1), "%"))
}
