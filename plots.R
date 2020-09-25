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

plot_yoy_ts_concentrations <- function(d.omi, countries, radius_km=50, year_min=NULL){

  year_min <- ifelse(is.null(year_min), min(lubridate::year(d.omi$date)), year_min)

  d <- d.omi %>%
    filter(radius_km==!!radius_km,
           SOURCETY != "Volcano") %>%
    mutate(COUNTRY = ifelse(COUNTRY %in% countries, COUNTRY, 'Others')) %>%
    filter(COUNTRY %in% countries) %>% # Remove Others if not in list
    group_by(COUNTRY, date, year=lubridate::year(date), radius_km) %>%
    dplyr::summarise(value=mean(value, na.rm=T)) %>%
    filter(year>=year_min) %>%
    ungroup() %>%
    rcrea::utils.rolling_average("day",30,"value", min_values = 15) %>%
    mutate(date = `year<-`(date, 0))

  # Adding yoy ratio
  d.yoy <- d %>% group_by(COUNTRY, date) %>%
    arrange(year) %>%
    mutate(value=ifelse(lag(year)==year-1,
                      (value -lag(value))/lag(value) ,
                      NA
                      )) %>%
    rcrea::utils.rolling_average("day",30,"value", min_values = 15)


  ggplot(d.yoy) +
    geom_line(aes(date, value, col=factor(year, ordered=T))) +
    facet_wrap (~COUNTRY, scales="free_y") +
    scale_color_brewer(palette="Paired", name="Year") +
    labs(title="SO2 column density around emission hotspots - 2020 vs 2019",
         subtitle="30-day running average",
         x=NULL,
         y=NULL) +
    scale_y_continuous(labels=scales::percent)+
    theme_light()
}

plot_ts_concentrations <- function(d.omi, countries, radius_km=50, year_min=NULL){

  year_min <- ifelse(is.null(year_min), min(lubridate::year(d.omi$date)), year_min)

  d <- d.omi %>%
    filter(radius_km==!!radius_km,
           SOURCETY != "Volcano") %>%
    mutate(COUNTRY = ifelse(COUNTRY %in% countries, COUNTRY, 'Others')) %>%
    filter(COUNTRY %in% countries) %>% # Remove Others if not in list
    group_by(COUNTRY, date, year=lubridate::year(date), radius_km) %>%
    dplyr::summarise(value=mean(value, na.rm=T)) %>%
    filter(year>=year_min) %>%
    ungroup() %>%
    rcrea::utils.rolling_average("day",30,"value") %>%
    mutate(date = `year<-`(date, 0))

  ggplot(d) +
    geom_line(aes(date, value, col=factor(year, ordered=T))) +
    facet_wrap (~COUNTRY, scales="free_y") +
    scale_color_brewer(palette="Paired", name="Year") +
    labs(title="SO2 column density around emission hotspots",
         subtitle="30-day running average",
         x=NULL,
         y="SO2 conentration [Dobson Unit]") +
    theme_light()
}

plot_yoy_concentrations <- function(d.omi, date_from=NULL, date_to=NULL, countries=NULL, radius_km=50, year_min=NULL, cutoff_date=NULL){


  # Two versions: whole year, offsetted by cutoff_date
  if(!is.null(cutoff_date)){
    date_to_year<- function(date){
      lubridate::year(date - lubridate::days(lubridate::yday(cutoff_date)) + lubridate::years(1))
    }
    period_name <- paste0(format(lubridate::date(cutoff_date)+lubridate::days(1), "%B"),
                          "-",
                          format(lubridate::date(cutoff_date), "%B"))
  }else{
    # Or between date_from and date_to
    date_to_year <- lubridate::year
    d.omi <- d.omi %>% filter(lubridate::yday(date) >= lubridate::yday(date_from),
                          lubridate::yday(date) <= lubridate::yday(date_to))
    period_name <- paste0(format(lubridate::date(date_from), "%B"),
                          "-",
                          format(lubridate::date(date_to), "%B"))
  }



  d.omi.country <- d.omi %>% filter(
           radius_km==!!radius_km,
           SOURCETY != "Volcano") %>%
    mutate(COUNTRY = ifelse(COUNTRY %in% countries, COUNTRY, 'Others')) %>%
    filter(COUNTRY %in% countries) %>% # Remove Others if not in list
    # # First group by hotstpot
    # group_by(NUMBER, SOURCETY, COUNTRY, radius_km, year=lubridate::year(date)) %>%
    # # First group by date (to mimic running365 plot)
    group_by(date, COUNTRY, radius_km, year=date_to_year(date)) %>%
    dplyr::summarise(value=mean(value, na.rm=T)) %>%
    # Then by country
    group_by(COUNTRY, year, radius_km) %>%
    dplyr::summarise(value=mean(value, na.rm=T))

  t.yoy.country <- utils.table_yoy_concentrations(d.omi.country,
                                                  group_by_cols=c("COUNTRY"))



  year_min <- ifelse(is.null(year_min), min(t.yoy.country$year), year_min)

  ggplot(t.yoy.country %>% filter(year>=year_min)) +
    geom_bar(aes(y=diff_yoy,x=year),
             stat="identity",
              fill="darkred") +
    facet_wrap(~COUNTRY, scales = "free_y") +
    geom_hline(yintercept=0) +
    theme_light() +
    # rcrea::theme_crea() +
    scale_x_continuous(breaks=scales::pretty_breaks()) +
    scale_y_continuous(labels=scales::percent) +
    labs(title=paste0("Year-on-year variation of SO2 concentration for ", period_name, " period"),
         caption="Source: OMI SO2 column amount at a 50km radius around anthropogenic emission hotspots.",
         y="Year-on-year difference [Dobson Unit]",
         x=NULL)
}


plot_yoy_concentrations_sectors <- function(d.omi, date_from=NULL, date_to=NULL, countries=NULL, radius_km=50, year_min=NULL, cutoff_date=NULL){

  # Two versions: whole year, offsetted by cutoff_date
  if(!is.null(cutoff_date)){
    date_to_year<- function(date){
      lubridate::year(date - lubridate::days(lubridate::yday(cutoff_date)) + lubridate::years(1))
    }
    period_name <- paste0(format(lubridate::date(cutoff_date)+lubridate::days(1), "%B"),
                          "-",
                          format(lubridate::date(cutoff_date), "%B"))
  }else{
    # Or between date_from and date_to
    date_to_year <- lubridate::year
    d.omi <- d.omi %>% filter(lubridate::yday(date) >= lubridate::yday(date_from),
                              lubridate::yday(date) <= lubridate::yday(date_to))
    period_name <- paste0(format(lubridate::date(date_from), "%B"),
                          "-",
                          format(lubridate::date(date_to), "%B"))
  }


  d.omi.country <- d.omi %>%
    filter(radius_km==!!radius_km,
           SOURCETY != "Volcano") %>%
    # # First group by hotstpot
    # group_by(NUMBER, SOURCETY, COUNTRY, radius_km, year=lubridate::year(date)) %>%
    # # First group by date (to mimic running365 plot)
    group_by(date, SOURCETY, radius_km, year=date_to_year(date)) %>%
    dplyr::summarise(value=mean(value, na.rm=T)) %>%
    # Then by country
    group_by(SOURCETY, year, radius_km) %>%
    dplyr::summarise(value=mean(value, na.rm=T))

  t.yoy.sector <- utils.table_yoy_concentrations(d.omi.country,
                                                  group_by_cols=c("SOURCETY"))

  year_min <- ifelse(is.null(year_min), min(t.yoy.sector$year), year_min)

  ggplot(t.yoy.sector %>% filter(year>=year_min)) +
    geom_bar(aes(y=diff_yoy,x=year),
             stat="identity",
             fill="darkred") +
    facet_wrap(~SOURCETY, scales = "free_y") +
    geom_hline(yintercept=0) +
    theme_light() +
    # rcrea::theme_crea() +
    scale_x_continuous(breaks=scales::pretty_breaks()) +
    scale_y_continuous(labels=scales::percent) +
    labs(title=paste0("Year-on-year variation of SO2 concentration for ", period_name, " period"),
         caption="Source: OMI SO2 column amount at a 50km radius around anthropogenic emission hotspots.",
         y="Year-on-year difference [Dobson Unit]",
         x=NULL)
}


plot.running2020_vs_2019 <- function(d.omi, d.measures.wide, radius_km, countries=NULL, percent=F, group_by_cols=c("COUNTRY")){

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


  # omi$value %>% (function(x) x %>% pmin(3) %>% pmax(0)) -> omi$value_filter
  # omi %<>% dplyr::rename(nofilter=value, filter=value_filter) %>% gather(filter, value, filter, nofilter)

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
    m <- mean(df$value[year(df$date)==2019], na.rm=T)
    if(percent){
      df$rmean = (rollapplyr(df$value, 365, mean, na.rm=T, fill=NA) - m)/m
    }else{
      df$rmean = (rollapplyr(df$value, 365, mean, na.rm=T, fill=NA) - m)
    }
    df
  }

  omi %>% filter(radius_km == !!radius_km, SOURCETY != 'Volcano') %>%
    mutate(COUNTRY = ifelse(COUNTRY %in% countries, COUNTRY, 'Others')) %>%
    filter(COUNTRY %in% countries) %>% # Remove Others if not in list
    group_by_at(c(group_by_cols, "date")) %>%
    summarise_at('value', mean, na.rm=T) %>%
    ddply(group_by_cols, roll_fn) ->
    rmean

  d <- rmean %>% filter(date>='2020-01-01')
  chg_colors <- c("#35416C", "#8CC9D0", "darkgray", "#CC0000", "#990000")
  ymax <- max(abs(d$rmean))
  p <- ggplot(d, aes(date, rmean, col=rmean)) +
    geom_line(size=1, show.legend = F) +
    facet_wrap(as.formula(paste("~", paste(group_by_cols, collapse = "+")))) +
    scale_color_gradientn(colors = chg_colors, limits=c(-ymax,ymax)) +
    # scale_color_distiller(palette="RdBu", limits=c(-0.035,0.035)) +
    # rcrea::CREAtheme.scale_color_crea_c('change', bias=.4) +
    theme_light() +
    labs(title="SO2 column amount around anthropogenic sources - 2020 vs 2019",
         caption="Source: OMI SO2 column amount at a 50km radius around anthropogenic emission hotspots.",
         x=NULL)

  if(percent){
    p <- p + scale_y_continuous(labels=scales::percent) +
      labs(subtitle="365-day running mean in 2020 vs 2019 average",y=NULL)
  }else{
    p <- p + labs(subtitle="365-day running mean in 2020 minus 2019 average",y="∆ column amount [Dobson Unit]")
  }
  return(p)
}




plot_ts <- function(d.omi, radius_km, group_by_cols, countries, year_min=NULL, running_days=365){

  year_min <- ifelse(is.null(year_min), min(lubridate::year(d.omi$date), na.rm=T), year_min)

  if(!"year" %in% names(d.omi)){
    d.omi <- d.omi %>%
      mutate(year=lubridate::year(date))
  }

  omi_rmean <- d.omi %>%
    filter(radius_km == !!radius_km,
           year >= year_min-1) %>%
    mutate(COUNTRY = ifelse(COUNTRY %in% countries, COUNTRY, 'Others')) %>%
    filter(COUNTRY %in% countries) %>% # Remove Others if not in list
    group_by_at(c(group_by_cols, "date")) %>%
    summarise_at('value', mean, na.rm=T) %>%
    rcrea::utils.rolling_average("day", running_days, "value")

  ggplot(omi_rmean) +
    geom_line(aes(date, value)) +
    facet_wrap(as.formula(paste("~", paste(group_by_cols, collapse = "+"))), scales="free_y") +
    theme_light() +
    ylim(0, NA) +
    xlim(year_min, 2021) +
    expand_limits(x = 0) +
    labs(title="SO2 column amount around anthropogenic sources",
         subtitle=paste0(running_days,"-day running average"),
         caption="Source: OMI SO2 column amount at a 50km radius around anthropogenic emission hotspots.",
         y= "Column amount [Dobson Unit]",
         x=NULL)

}


