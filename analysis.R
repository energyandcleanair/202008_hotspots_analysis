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

extract_omi_data <- function(){

  # Specific requirements to parallelize value extraction
  require(rgdal) #install.packages("rgdal")
  require(velox) #remotes::install_github("hunzikp/velox")
  require(rgis) #remotes::install_github("Pakillo/rgis")

  print("Creating OMI value files. Can take a few hours if not days...")
  radii_km <- c(2, 20, 200)
  d.measures <- utils.read_points()

  pts <- do.call("rbind", lapply(radii_km, utils.buffer, points=d.measures.wide))

  # Get values
  files <- list.files("data",pattern="\\.nc4$", full.names = T)

  d <- do.call("rbind", pbapply::pblapply(files, utils.values_at_point, points=pts))
  d.omi <- d %>% dplyr::select(NUMBER, radius_km, value, date)

  saveRDS(d.omi, file_values)
}

if(!file.exists(file_values)){
  extract_omi_data()
}

d.omi <- readRDS(file_values) %>%
  mutate(value = replace_na(value, 0),
         radius_km = factor(radius_km, ordered=T))

d.omi.year <- d.omi %>%
  group_by(NUMBER, year=lubridate::year(date), radius_km) %>%
  summarise(value_omi=mean(value, na.rm=T),
            count_omi_sgt_0=sum(value>0))

d.measures.year <- tibble(d.measures.wide) %>%
  dplyr:: select( grep( "NUMBER|SOURCETY|ELEVATION|^y" , names(d.measures.wide))) %>%
  tidyr::pivot_longer(-c(NUMBER,SOURCETY,ELEVATION), names_to="year", names_prefix = "y", values_to="value_original") %>%
  mutate(year=as.numeric(year))

d.all.year <- inner_join(d.omi.year, d.measures.year, by=c("NUMBER", "year"))
saveRDS(d.all.year, file.path("results","hotspots_omi_vs_measures.RDS"))


# Predictions: emission from concentration  -------------------------------------------

d.omi.pred <- utils.prediction_ytd(d.all.year, d.omi, cut_date="2020-08-01")
saveRDS(d.omi.pred, file.path("results","data","omi_predictions.RDS"))
write.csv(d.omi.pred, file.path("results","data","omi_predictions.csv"), row.names = F)

(p <- ggplot(d.omi.pred %>% filter(year_offsetted>=2008, !is.na(SOURCETY))) +
   geom_boxplot(aes(x=SOURCETY, y=ratio_yoy, color=year_offsetted_text), outlier.size = 0.2) +
   labs(title="Y-o-y (August-July) variation of estimated hotspot SO2 emissions",
        x=NULL, y=NULL) +
   scale_y_continuous(labels = scales::percent) +
   scale_color_discrete(name="")+
   rcrea::theme_crea())

(p_zoom <- p + scale_y_continuous(labels = scales::percent, limits=c(NA,1)) +
    labs(subtitle="Values above 100% not shown"))

ggsave(file.path("results","plots","prediction_2020.png"), plot=p, width=12, height=10)
ggsave(file.path("results","plots","prediction_2020_zoom.png"), plot=p_zoom, width=12, height=10)


# Other exploratory plots -------------------------------------------------

(p_iqr <- ggplot(d.omi.pred %>%filter(year_offsetted>=2008, !is.na(SOURCETY)),
             aes(x=year_offsetted, y=ratio_yoy)) +
    geom_smooth(stat = 'summary', color = 'blue', fill = 'red', alpha = 0.2,
                fun.data = median_hilow, fun.args = list(conf.int = 0.5)) +
    # geom_smooth(stat = 'summary', color = 'blue', fill = 'red', alpha = 0.2,
    #             fun.data = median_hilow, fun.args = list(conf.int = 0.90)) +
    facet_wrap(~SOURCETY) +
    labs(title="Yoy (August-July) variation of estimated hotspot SO2 emissions",
         x=NULL, y=NULL,
         caption="Red area represents the interquartile range") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks=seq(2008, 2020, 2), expand=c(0,0)) +
    scale_color_discrete(name="")+
    rcrea::theme_crea())

ggsave(file.path("results","plots","prediction_2020_iqr.png"), plot=p_iqr, width=14, height=10)

