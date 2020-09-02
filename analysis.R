require(dplyr)
require(tidyr)
require(sf)
require(stringr)
require(pbapply)
require(raster)
require(ggplot2)
require(magrittr)
require(rcrea)
require(Hmisc)

source('./utils.R')
source('./plots.R')
source('./extract.R')
file_values <- file.path("results", "values.RDS")

dir.create(file.path("results","plots"), recursive=T, showWarnings=F)
dir.create(file.path("results","data"), recursive=T, showWarnings=F)


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
  extract_omi_data()
}

d.omi <- readRDS(file_values) %>%
  mutate(
    # value = replace_na(value, 0), #THAT WAS THE BIG MISTAKE... KEEP COMMENTED
    radius_km = factor(radius_km, ordered=T)) %>%
  filter(value<=5, value>-10) #"We only include data with values between −10 and 5 DU" in NASA inversion paper:
  # Qu, Z., Henze, D. K., Capps, S. L., Wang, Y., Xu, X., Wang, J., & Keller, M. (2017). Monthly top-down NOx
  # emissions for China (2005-2012): A hybrid inversion method and trend analysis. Journal of Geophysical Research,

d.omi.year <- d.omi %>%
  group_by(NUMBER, year=lubridate::year(date), radius_km) %>%
  summarise(value_omi=mean(value, na.rm=T),
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
                          values_to="sigma_original") %>%
      mutate(year=as.numeric(year))
  )


d.all.year <- inner_join(d.omi.year, d.measures.year, by=c("NUMBER", "year"))
saveRDS(d.all.year, file.path("results","hotspots_omi_vs_measures.RDS"))


# Predictions: emission from concentration  -------------------------------------------
# Create prediction data set on offsetted-year values
date_to_year_offseted <- function(date){
  lubridate::year(date - lubridate::days(lubridate::yday("0000-08-01")) + lubridate::years(1))
}

d.omi.pred <- utils.prediction_ytd(d.all.year, d.omi, date_to_year=date_to_year_offseted)
saveRDS(d.omi.pred, file.path("results","data","omi_predictions.RDS"))
write.csv(d.omi.pred, file.path("results","data","omi_predictions.csv"), row.names = F)

(p1 <- plot_total_emissions(d.omi.pred, period_name="August-July"))
ggsave(file.path("results","plots","total_emissions_2020.png"), plot=p1, width=12, height=6)

(p2 <- plot_yoy_variations(d.omi.pred, period_name="August-July"))
ggsave(file.path("results","plots","yoy_variations_2020.png"), plot=p2, width=12, height=6)

utils.table_yoy(d.omi.pred)
t.yoy <- utils.country_table_yoy(d.omi.pred)
write.csv(t.yoy, file.path("results","data","yoy.augjuly.csv"), row.names=F)

t.yoy.sector <- utils.country_table_yoy(d.omi.pred, group_by_cols = c("COUNTRY","SOURCETY"))
write.csv(t.yoy.sector, file.path("results","data","yoy.augjuly.sector.csv"), row.names=F)


# Jan - July ------------------------------------------------------------
date_to_year_july <- function(date){
  ifelse(lubridate::yday(date)<lubridate::yday("0000-08-01"),
         lubridate::year(date),
         NA)
}
d.omi.pred.july <- utils.prediction_ytd(d.all.year, d.omi, date_to_year=date_to_year_july)

(p1 <- plot_total_emissions(d.omi.pred.july, period_name="January-July"))
ggsave(file.path("results","plots","total_emissions_2020_july.png"), plot=p1, width=12, height=6)

(p2 <- plot_yoy_variations(d.omi.pred.july, period_name="January-July"))
ggsave(file.path("results","plots","yoy_variations_2020_july.png"), plot=p2, width=12, height=6)

utils.table_yoy(d.omi.pred.july)

t.yoy.july <- utils.country_table_yoy(d.omi.pred.july)
write.csv(t.yoy.july, file.path("results","data","yoy.janjuly.csv"), row.names=F)

t.yoy.july.sector <- utils.country_table_yoy(d.omi.pred.july, group_by_cols = c("COUNTRY","SOURCETY"))
write.csv(t.yoy.july.sector, file.path("results","data","yoy.janjuly.sector.csv"), row.names=F)


# Other plots -------------------------------------------------------------
#
# (p <- ggplot(d.omi.pred %>% filter(year_offsetted>=2008, !is.na(SOURCETY))) +
#    geom_boxplot(aes(x=SOURCETY, y=ratio_yoy, color=year_offsetted_text), outlier.size = 0.2) +
#    labs(title="Y-o-y (August-July) variation of estimated hotspot SO2 emissions",
#         x=NULL, y=NULL) +
#    scale_y_continuous(labels = scales::percent) +
#    scale_color_discrete(name="")+
#    rcrea::theme_crea())
#
# (p_zoom <- p + scale_y_continuous(labels = scales::percent, limits=c(NA,1)) +
#     labs(subtitle="Values above 100% not shown"))
#
# ggsave(file.path("results","plots","prediction_2020.png"), plot=p, width=12, height=10)
# ggsave(file.path("results","plots","prediction_2020_zoom.png"), plot=p_zoom, width=12, height=10)
#
#
# # Other exploratory plots -------------------------------------------------
#
# (p_iqr <- ggplot(d.omi.pred %>%filter(year_offsetted>=2008, !is.na(SOURCETY)),
#              aes(x=year_offsetted, y=ratio_yoy)) +
#     geom_smooth(stat = 'summary', color = 'blue', fill = 'red', alpha = 0.2,
#                 fun.data = median_hilow, fun.args = list(conf.int = 0.5)) +
#     # geom_smooth(stat = 'summary', color = 'blue', fill = 'red', alpha = 0.2,
#     #             fun.data = median_hilow, fun.args = list(conf.int = 0.90)) +
#     facet_wrap(~SOURCETY) +
#     labs(title="Yoy (August-July) variation of estimated hotspot SO2 emissions",
#          x=NULL, y=NULL,
#          caption="Red area represents the interquartile range") +
#     scale_y_continuous(labels = scales::percent) +
#     scale_x_continuous(breaks=seq(2008, 2020, 2), expand=c(0,0)) +
#     scale_color_discrete(name="")+
#     rcrea::theme_crea() +
#    theme(panel.spacing.x=unit(1.5, "lines")))
#
# ggsave(file.path("results","plots","prediction_2020_iqr.png"), plot=p_iqr, width=10, height=6)

