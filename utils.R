utils.read_points <- function(){

  p <- readxl::read_xlsx(file.path("data", "OMI_Catalogue_Emissions_2005-2019_ILS-update.xlsx"),
                        range="A3:BQ591")

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

utils.prediction_ytd <- function(d.all.year, d.omi, cut_date="2020-08-01"){

  # We train models on d.all.year for every calendar year (the only periods we have Measures data for)
  # And then apply to different year cutting (e.g. August to July)
  d <- d.all.year
  d  %<>% filter(SOURCETY != "Volcano")
  d  %<>% distinct(NUMBER, year, value_original) %>%
    group_by(NUMBER) %>%
    summarise(value_original_mean = mean(value_original, na.rm=T)) %>%
    full_join(d, .)
  dw <- d %>%
    mutate(radius_km = paste0('r', radius_km)) %>%
    dplyr::select(-count_omi_sgt_0) %>%
    spread(radius_km, value_omi) %>%
    mutate(variation=value_original-value_original_mean)

  lm(value_original ~ value_original_mean + (r50 + r200):(SOURCETY + ELEVATION), data=dw) -> m
  summary(m)

  # Other attempts or baselines
  # summary(lm(value_original ~ value_original_mean + (r50 + r200)*(SOURCETY + ELEVATION), data=dw))
  # summary(lm(value_original ~ value_original_mean, data=dw))
  # summary(lm(variation ~ (r50 + r200):(SOURCETY + ELEVATION), data=dw))
  # summary(lm(variation ~ factor(NUMBER) + (r50 + r200):(SOURCETY + ELEVATION), data=dw))
  # summary(lm(value_original ~ (r50 + r200):(SOURCETY + ELEVATION), data=dw))
  # summary(lm(value_original ~ (r10 + r20 + r50 + r100 + r200):(SOURCETY + ELEVATION), data=dw))
  predict(m, dw) -> dw$value_original_pred

  # Plot some performance or simple correlation charts
  plt_all <- dw %>%
    ggplot(aes(value_original, value_original_pred, color=SOURCETY)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~NUMBER, scales='free')

  ggsave(file.path("results","plots","pred_vs_observed.png"), plot=plt_all, width=28, height=26, scale=1.3)


  plt_pearson <- dw %>%
    group_by(NUMBER, SOURCETY) %>%
    summarise(pearson=cor(value_original, value_original_pred, method = c("pearson"))) %>%
    ggplot() +
    geom_boxplot(aes(SOURCETY, pearson)) +
    theme_crea() +
    labs(subtitle="Correlation between predicted and original SO2 emissions",
              y="Pearson's r",
              x=NULL)
  ggsave(file.path("results","plots","pearson_sourcetype.png"), plot=plt_pearson, width=12, height=10)

  # Create prediction data set on offsetted-year values
  date_to_year_offseted <- function(date){
    lubridate::year(date - lubridate::days(lubridate::yday(cut_date)) + lubridate::years(1))
  }

  d.omi.pred <- d.omi %>%
    mutate(year_offsetted= date_to_year_offseted(date)) %>%
    filter(year_offsetted > lubridate::year(min(date)), year_offsetted < 2021) %>%
    group_by(NUMBER, year_offsetted, radius_km) %>%
    summarise(value_omi=mean(value, na.rm=T), count=n()) %>%
    mutate(radius_km = paste0('r', radius_km)) %>%
    spread(radius_km, value_omi) %>%
    left_join(d %>% ungroup() %>% distinct(NUMBER, SOURCETY, ELEVATION, value_original_mean))

  d.omi.pred$predicted <- pmax(predict(m, d.omi.pred),0)

  d.omi.pred <- d.omi.pred %>%
    group_by(NUMBER) %>%
    arrange(year_offsetted) %>%
    mutate(ratio_yoy=ifelse(lag(year_offsetted)==year_offsetted-1,
                            (predicted-lag(predicted))/lag(predicted),
                            NA)) %>%
    mutate(year_offsetted_text = paste(year_offsetted,"/",year_offsetted-1))

  d.omi.pred$ratio_yoy[is.infinite(d.omi.pred$ratio_yoy)] <- 100

  return(d.omi.pred)
}
