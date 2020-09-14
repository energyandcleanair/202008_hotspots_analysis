require(plyr)
require(dplyr)
require(tidyr); require(magrittr); require(readxl)
require(sf)
require(stringr)
require(pbapply)
require(raster)
require(ggplot2)
require(magrittr)
require(rcrea)
require(Hmisc)
require(zoo)

source('./utils.R')
source('./plots.R')
source('./extract.R')
file_values <- file.path("results", "values.RDS")
file_values_202008 <- file.path("results", "values_202008.RDS")
sel <- dplyr::select

run_concentrations <- function(date_from, date_to, experiment_name, radius_km=50, n_mad=5){

  dir_results <- file.path("results", experiment_name)
  dir.create(dir_results, recursive=T, showWarnings = F)

  d.measures.wide <- tibble(utils.read_points())

  d.omi <- readRDS(file_values) %>%
    filter(date < "2020-08-01",
           radius_km==!!radius_km) %>%
    rbind(readRDS(file_values_202008)) %>%
    left_join(d.measures.wide %>% sel(NUMBER, SOURCETY, COUNTRY, contains("ITUD"))) %>%
    utils.merge_europe() %>%
    filter(SOURCETY!="Volcano")

  d.omi.filtered <- d.omi %>%
    group_by(NUMBER, SOURCETY, COUNTRY, radius_km) %>%
    mutate(value=utils.mad_filter(value, n_mad=n_mad, saturate=T))

  countries <- c("India", "Russia", "China", "Mexico", "South Africa", "Saudi Arabia", "Europe", "Australia")

  t.yoy.country <- utils.table_yoy_concentrations(d.omi.filtered, group_by_cols = c("COUNTRY"))
  write.csv(t.yoy.country, file.path(dir_results, "t.yoy.country.csv"), row.names=F)

  t.yoy.country.sector <- utils.table_yoy_concentrations(d.omi.filtered, group_by_cols = c("COUNTRY", "SOURCETY"))
  write.csv(t.yoy.country.sector, file.path(dir_results, "t.yoy.country.sector.csv"), row.names=F)

  t.yoy.sector <- utils.table_yoy_concentrations(d.omi.filtered, group_by_cols = c("SOURCETY"))
  write.csv(t.yoy.sector, file.path(dir_results, "t.yoy.sector.csv"), row.names=F)

#
#
#   (p <- plot.running2020_vs_2019(d.omi.filtered, d.measures.wide, countries=countries, radius_km=radius_km))
#   ggsave(filename=file.path(dir_results, "running2020_vs_2019.png"), plot=p, width=12, height=10)
#
#   (p <- plot_ts_concentrations(d.omi.filtered, countries=countries, radius_km=radius_km, year_min=2010))
#   ggsave(filename=file.path(dir_results, "ts_2010_2020.png"), plot=p, width=12, height=10)
#
#   (p <- plot_yoy_concentrations(d.omi.filtered, date_from, date_to, countries=countries, radius_km=radius_km, year_min=2010))
#   ggsave(filename=file.path(dir_results, "yoy_2010_2020.png"), plot=p, width=12, height=10)
#
#   (p <- plot_yoy_concentrations(d.omi.filtered, date_from, date_to, countries=countries, radius_km=radius_km, year_min=2015))
#     ggsave(filename=file.path(dir_results, "yoy_2015_2020.png"), plot=p, width=12, height=10)

}


run_predictions <- function(formula, interval, experiment_name, cutoff_date, lm_per_source, fn_aggregate_omi=mean){

  # formula: the formula to be used in lm
  # interval: the type of CI used in predict: either 'confidence' or 'prediction'
  # experiment_name: used for resut subfolder
  # cutoff_date: when are we cutting years e.g. 0000-09-01 for Sep -> Aug predictions
  # lm_per_source: whether to apply one model per emissino source (lm_per_source=T)
  #               or one model for all sources together
  # fn_aggregate_omi: how do we aggregate OMI emission data for each source per year (e.g. mean or sum)


  # Save config in folder
  dir_results <- file.path("results", experiment_name)
  dir.create(dir_results, recursive=T, showWarnings = F)
  utils.save_config(dir_results, formula, interval, cutoff_date)

  # Explanation -------------------------------------------------------------
  # 'measures' refers to NASA emission data
  # 'omi' refers to (NASA) concentration data
  # 'all' refers to both combined

  # value,RDS has been generated previously on GCP
  # from raw OMI files through the create_omi function.

  # Getting raw values ------------------------------------------------------

  # Reading NASA emission excel sheet (updated by Isabella)
  d.measures.wide <- utils.read_points()

  if(!file.exists(file_values)){
    # Extract OMI values from NC files. Results were pre-computed
    # and stored in "results" folder
    extract_omi_data()
  }

  d.omi <- readRDS(file_values) %>%
    filter(date < "2020-08-01") %>%
    rbind(readRDS(file_values_202008))

  # "We only include data with values between −10 and 5 DU" in NASA inversion paper:
  # Qu, Z., Henze, D. K., Capps, S. L., Wang, Y., Xu, X., Wang, J., & Keller, M. (2017).
  # Monthly top-down NOx emissions for China (2005-2012):
  # A hybrid inversion method and trend analysis. Journal of Geophysical Research
  d.omi <- d.omi %>%
    mutate(
      # value = replace_na(value, 0), #THAT WAS THE BIG MISTAKE... KEEP COMMENTED
      radius_km = factor(radius_km, ordered=T)) %>%
    filter(value<=5, value>-10) #

  d.omi.year <- d.omi %>%
    group_by(NUMBER, year=lubridate::year(date), radius_km) %>%
    summarise(value_omi=fn_aggregate_omi(value, na.rm=T),
              count_omi=sum(!is.na(value)))

  d.measures.year <- tibble(d.measures.wide) %>%
    dplyr:: select( grep( "NUMBER|SOURCETY|COUNTRY|ELEVATION|^y" , names(d.measures.wide))) %>%
    tidyr::pivot_longer(-c(NUMBER,SOURCETY,COUNTRY,ELEVATION),
                        names_to="year",
                        names_prefix = "y",
                        values_to="value_original") %>%
    mutate(year=as.numeric(year)) %>%
    left_join(
      tibble(d.measures.wide) %>%
        dplyr:: select( grep( "NUMBER|SOURCETY|COUNTRY|ELEVATION|^s" , names(d.measures.wide))) %>%
        tidyr::pivot_longer(-c(NUMBER,SOURCETY,COUNTRY,ELEVATION),
                            names_to="year",
                            names_prefix = "s",
                            values_to="sigma_nasa") %>%
        mutate(year=as.numeric(year))
    )


  d.all.year <- inner_join(d.omi.year, d.measures.year, by=c("NUMBER", "year")) %>%
    filter(SOURCETY != "Volcano")

  saveRDS(d.all.year, file.path("results","hotspots_omi_vs_measures.RDS"))

  # Predictions: emission from concentration  -------------------------------------------
  # Create prediction data set on offsetted-year values
  if(!is.null(cutoff_date)){
    date_to_year_offseted <- function(date){
      lubridate::year(date - lubridate::days(lubridate::yday(cutoff_date)) + lubridate::years(1))
    }
  }else{
    date_to_year_offseted <- lubridate::year
  }


  r <- utils.prediction_ytd(d.all.year,
                            d.omi,
                            date_to_year=date_to_year_offseted,
                            formula=formula,
                            interval=interval,
                            dir_results=dir_results,
                            lm_per_source=lm_per_source)

  d.omi.pred <- r$d.omi.pred
  m <- r$m
  rm(r)

  saveRDS(d.omi.pred, file.path(dir_results, "omi_predictions.RDS"))
  write.csv(d.omi.pred, file.path(dir_results, "omi_predictions.csv"), row.names = F)

  (p0 <- plot_total_emissions(d.omi.pred, period_name="September-August", title="Global SO2 emissions", group_by_cols=c()))
  ggsave(file.path(dir_results, "total_emissions_global.png"), plot=p0, width=12, height=6)

  (p1 <- plot_total_emissions(d.omi.pred, period_name="September-August", title="SO2 emissions across sectors"))
  ggsave(file.path(dir_results, "total_emissions_global.png"), plot=p1, width=12, height=6)

  (p2 <- plot_yoy_variations(d.omi.pred, period_name="September-August", title="Y-o-y variations of SO2 emissions across sectors"))
  ggsave(file.path(dir_results, "yoy_variations_sectors.png"), plot=p2, width=12, height=6)

  (p3 <- plot_yoy_variations(d.omi.pred, period_name="September-August", title="Y-o-y variations of global SO2 emissions", group_by_cols=c()))
  ggsave(file.path(dir_results, "yoy_variations_global.png"), plot=p3, width=12, height=6)

  t.total.country <- utils.table_total(d.omi.pred, group_by_cols = c("COUNTRY"))
  write.csv(t.total.country, file.path(dir_results, "total.country.csv"), row.names=F)

  t.total.sector <- utils.table_total(d.omi.pred, group_by_cols = c("SOURCETY"))
  write.csv(t.total.sector, file.path(dir_results, "total.sector.csv"), row.names=F)

  t.yoy.global <- utils.table_yoy(d.omi.pred, group_by_cols = c())
  write.csv(t.yoy.global, file.path(dir_results, "yoy.global.csv"), row.names=F)

  t.yoy.country <- utils.table_yoy(d.omi.pred, group_by_cols = c("COUNTRY"))
  write.csv(t.yoy.country, file.path(dir_results, "yoy.country.csv"), row.names=F)

  t.yoy <- utils.table_yoy_w_europe(d.omi.pred)
  write.csv(t.yoy, file.path(dir_results,"yoy.top10.csv"), row.names=F)

  t.yoy.sector <- utils.table_yoy(d.omi.pred, group_by_cols = c("COUNTRY","SOURCETY"))
  write.csv(t.yoy.sector, file.path(dir_results, "yoy.sector.csv"), row.names=F)

}

