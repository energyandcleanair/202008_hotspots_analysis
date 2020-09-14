plot_total_emissions <- function(d.omi.pred, period_name, title, group_by_cols = c("SOURCETY")){

  # Build data with confidence interval
  d.plot <- utils.table_total(d.omi.pred, group_by_cols = c("SOURCETY"))

  p <- ggplot(d.plot) +
    geom_ribbon(aes(x=year_offsetted, ymin = lwr, ymax = upr), fill="darkred", alpha=0.2) +
    geom_line(aes(year_offsetted, value), color="darkred") +
    # geom_line(aes(ratio_yoy, value), color="darkblue", linetype="dashed") +
    rcrea::theme_crea() +
    scale_x_continuous(breaks=seq(2006, 2020, 2), expand=c(0,0)) +
    scale_y_continuous(limits=c(0,NA), expand=c(0,1000)) +
    labs(y="SO2 emission [kton/yr]", x=NULL,
         title=title,
         subtitle=paste("Estimations for the", period_name, "periods across 588 identified hotspots"),
         caption="Source: CREA estimation based on NASA OMI and MEaSUREs. Red area represents the 95% level of confidence.")

  if(length(group_by_cols)>0){
    p <- p + facet_wrap(as.formula(paste("~", paste(group_by_cols, collapse = "+"))))
  }
  p

}

plot_yoy_variations <- function(d.omi.pred, period_name, title, group_by_cols = c("SOURCETY")){

  d <- utils.table_yoy(d.omi.pred, group_by_cols=group_by_cols)

  if(identical(group_by_cols,c("SOURCETY"))){
    a <- aes(year_offsetted, diff_yoy, fill=SOURCETY)
  }else{
    a <- aes(year_offsetted, diff_yoy)
  }


  p <- ggplot(d) +
    geom_bar(a,  stat="identity", show.legend = FALSE) +
    # geom_point(aes(year_offsetted, ratio_yoy), color="darkred", size=0.8, alpha=0.5) +
    # geom_ribbon(aes(x=year_offsetted, ymin = ratio_yoy_lwr, ymax = ratio_yoy_upr), fill="darkred", alpha=0.2) +
    geom_errorbar(aes(x=year_offsetted,ymin=diff_yoy_lwr,ymax=diff_yoy_upr),width=.2,position=position_dodge(.9), color="#888888") +
    rcrea::theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d(palette="dramatic")+
    scale_x_continuous(breaks=seq(2000, 2020, 2), expand=c(0,0)) +
    scale_y_continuous(labels=scales::percent) +
    geom_hline(yintercept=0) +
    labs(y="Y-o-y SO2 emission", x=NULL,
         title=title,
         subtitle=paste("Estimations for the", period_name, "periods across 588 identified hotspots"),
         caption="Source: CREA estimation based on NASA OMI and MEaSUREs. Error bars represent the 95%-level confidence interval.")

  if(length(group_by_cols)>0){
    p <- p + facet_wrap(as.formula(paste("~", paste(group_by_cols, collapse = "+"))))
  }
  p
}

plot_all_correlations <- function(dw_plot){

 ggplot(dw_plot) +
   geom_point(aes(value_original, predicted)) +
   geom_abline(slope=1, intercept=0) +
   xlim(0,NA) +
   ylim(0, NA) +
   facet_wrap(~NUMBER, scales="free")
}

plot_ts_concentrations <- function(d.omi, countries, radius_km=50, year_min=NULL){
  d <- d.omi %>%
    filter(radius_km==!!radius_km,
           SOURCETY != "Volcano") %>%
    mutate(COUNTRY = ifelse(COUNTRY %in% countries, COUNTRY, 'Others')) %>%
    # First group by hotstpot
    group_by(NUMBER, SOURCETY, COUNTRY, radius_km, date, year=lubridate::year(date)) %>%
    dplyr::summarise(value=mean(value, na.rm=T)) %>%
    # Then by country
    group_by(COUNTRY, date, year, radius_km) %>%
    dplyr::summarise(value=mean(value, na.rm=T)) %>%
    mutate(date = `year<-`(date, 0))

  ggplot(d %>% filter(year>=2015) %>% rcrea::utils.rolling_average("day",30,"value") ) +
    geom_line(aes(date, value, col=factor(year, ordered=T))) +
    facet_wrap (~COUNTRY, scales="free_y") +
    scale_color_brewer(palette="Paired", name="Year") +
    labs(title="SO2 column density around emission hotspots",
         x=NULL,
         y="SO2 conentration [Dobson Unit]") +
    theme_light()
}

plot_yoy_concentrations <- function(d.omi, date_from, date_to, countries, radius_km=50, year_min=NULL){

  d.omi.country <- d.omi %>%
    filter(lubridate::yday(date) >= lubridate::yday(date_from),
           lubridate::yday(date) <= lubridate::yday(date_to),
           radius_km==!!radius_km,
           SOURCETY != "Volcano") %>%
    mutate(COUNTRY = ifelse(COUNTRY %in% countries, COUNTRY, 'Others')) %>%
    # # First group by hotstpot
    # group_by(NUMBER, SOURCETY, COUNTRY, radius_km, year=lubridate::year(date)) %>%
    # # First group by date (to mimic running365 plot)
    group_by(date, COUNTRY, radius_km, year=lubridate::year(date)) %>%
    dplyr::summarise(value=mean(value, na.rm=T)) %>%
    # Then by country
    group_by(COUNTRY, year, radius_km) %>%
    dplyr::summarise(value=mean(value, na.rm=T))

  t.yoy.country <- utils.table_yoy_concentrations(d.omi.country,
                                                  group_by_cols=c("COUNTRY"))

  period_name <- paste0(format(lubridate::date(date_from), "%B"),
                        "-",
                        format(lubridate::date(date_to), "%B"))

  year_min <- ifelse(is.null(year_min), min(t.yoy.country$year), year_min)

  ggplot(t.yoy.country %>% filter(year>=year_min)) +
    geom_bar(aes(y=diff_yoy,x=year),
             stat="identity",
              fill="darkred") +
    facet_wrap(~COUNTRY, scales = "free_y") +
    geom_hline(yintercept=0) +
    theme_light() +
    # rcrea::theme_crea() +
    scale_y_continuous(labels=scales::percent) +
    labs(title=paste0("Year-on-year variation of SO2 concentration for ", period_name, " period"),
         caption="Source: CREA based on OMI",
         y="Year-on-year difference [Dobson Unit]",
         x=NULL)
}


plot.running2020_vs_2019 <- function(d.omi, d.measures.wide, radius_km, countries=NULL){

  # require(tidyverse); require(magrittr); require(readxl)
  # require(lubridate)
  # require(zoo)

  sel = dplyr::select
  # Renaming to Lauri's convention
  omi <- d.omi
  meas <- d.measures.wide

  # meas %>% sel(NUMBER, SOURCETY, COUNTRY, contains("ITUD")) %>% full_join(omi) -> omi

  if(is.null(countries)){
    meas %>% filter(SOURCETY != 'Volcano') %>%
      group_by(COUNTRY) %>% summarise_at('y2019', sum, na.rm=T) %>% arrange(-y2019) %>%
      use_series(COUNTRY) -> countryrank
    countries <- countryrank[1:8]
  }


  omi$value %>% (function(x) x %>% pmin(3) %>% pmax(0)) -> omi$value_filter
  omi %<>% dplyr::rename(nofilter=value, filter=value_filter) %>% gather(filter, value, filter, nofilter)

  # omi %>% filter(month(date) %in% 3:8, radius_km == 50) %>% group_by(SOURCETY, year=year(date), filter) %>%
  #   summarise_at('value', mean, na.rm=T) -> omi_yearly
  #
  # omi_yearly %>% filter(!is.na(SOURCETY), filter=='filter') %>%
  #   ggplot(aes(year, value)) + geom_col() + facet_wrap(~SOURCETY)
  #
  # omi %>% filter(radius_km == !!radius_km) %>% group_by(SOURCETY, filter, date) %>%
  #   summarise_at('value', mean, na.rm=T) %>%
  #   ddply(.(SOURCETY, filter),
  #         function(df) {df$rmean = rollapplyr(df$value, 365, mean, na.rm=T, fill=NA); df}) ->
  #   omi_rmean
  #
  # omi_rmean %>% filter(!is.na(SOURCETY), filter=='filter') %>%
  #   ggplot(aes(date, rmean)) + geom_line() + facet_wrap(~SOURCETY)

  roll_fn <- function(df){
    df$rmean = rollapplyr(df$value, 365, mean, na.rm=T, fill=NA) -
      mean(df$value[year(df$date)==2019], na.rm=T)
    df
  }
  omi %>% filter(radius_km == !!radius_km, SOURCETY != 'Volcano') %>%
    mutate(COUNTRY = ifelse(COUNTRY %in% countries, COUNTRY, 'Others')) %>%
    group_by(COUNTRY, date, filter) %>%
    summarise_at('value', mean, na.rm=T) %>%
    ddply(.(COUNTRY, filter), roll_fn) ->
    country_rmean

  chg_colors <- c("#35416C", "#8CC9D0", "darkgray", "#CC0000", "#990000")
  country_rmean %>% filter(date>='2020-01-01', filter=='nofilter') %>%
    ggplot(aes(date, rmean, col=rmean)) + geom_line(size=1) + facet_wrap(~COUNTRY) +
    scale_color_gradientn(colors = chg_colors, limits=c(-0.035,0.035)) +
    # scale_color_distiller(palette="RdBu", limits=c(-0.035,0.035)) +
    # rcrea::CREAtheme.scale_color_crea_c('change', bias=.4) +
    theme_light() +
    labs(title="SO2 column density around anthropogenic sources - 2020 vs 2019",
         subtitle="365-day running mean in 2020 minus 2019 average",
         caption="Source: OMI SO2 tropospheric column density at a 50km radius around anthropogenic emission hotspots.",
         y= "∆ column density [Dobson Unit]",
         x=NULL)
}

