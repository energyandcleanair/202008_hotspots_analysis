plot_total_emissions <- function(d.omi.pred){
  ggplot(d.omi.pred %>%
                  filter(!is.na(SOURCETY), year_offsetted>=200) %>%
                  group_by(year_offsetted, SOURCETY) %>%
                  summarise(value=sum(predicted, na.rm=T),
                            lwr=sum(predicted_lwr, na.rm=T),
                            upr=sum(predicted_upr, na.rm=T)) %>%
                  mutate(ratio_yoy=ifelse(lag(year_offsetted)==year_offsetted-1,
                                          (value-lag(value))/lag(value),
                                          NA),
                         ratio_yoy_lwr=ifelse(lag(year_offsetted)==year_offsetted-1,
                                              (lwr-lag(value))/lag(value),
                                              NA),
                         ratio_yoy_upr=ifelse(lag(year_offsetted)==year_offsetted-1,
                                              (upr-lag(value))/lag(value),
                                              NA))
  ) +
    geom_ribbon(aes(x=year_offsetted, ymin = lwr, ymax = upr), fill="darkred", alpha=0.2) +
    geom_line(aes(year_offsetted, value), color="darkred") +
    # geom_line(aes(ratio_yoy, value), color="darkblue", linetype="dashed") +
    facet_wrap(~SOURCETY, scales="free") +
    rcrea::theme_crea() +
    scale_x_continuous(breaks=seq(2006, 2020, 2), expand=c(0,0)) +
    scale_y_continuous(limits=c(0,NA), expand=c(0,1000)) +
    labs(y="SO2 emission [kton/yr]", x=NULL,
         title="SO2 emissions across sectors",
         subtitle="Estimations for the August-to-July periods across 588 identified hotspots",
         caption="Source: CREA estimation based on NASA OMI and MEaSUREs. Red area represents the 95% level of confidence.")
}

plot_yoy_variations <- function(d.omi.pred){
ggplot(d.omi.pred %>%
                  filter(!is.na(SOURCETY), year_offsetted>=2000) %>%
                  group_by(year_offsetted, SOURCETY) %>%
                  summarise(value=sum(predicted, na.rm=T),
                            lwr=sum(predicted_lwr, na.rm=T),
                            upr=sum(predicted_upr, na.rm=T)) %>%
                  group_by(SOURCETY) %>%
                  arrange(year_offsetted) %>%
                  mutate(ratio_yoy=ifelse(lag(year_offsetted)==year_offsetted-1,
                                          (value-lag(value))/lag(value),
                                          NA),
                         ratio_yoy_lwr=ifelse(lag(year_offsetted)==year_offsetted-1,
                                              (lwr-lag(value))/lag(value),
                                              NA),
                         ratio_yoy_upr=ifelse(lag(year_offsetted)==year_offsetted-1,
                                              (upr-lag(value))/lag(value),
                                              NA))
  ) +
    geom_line(aes(year_offsetted, ratio_yoy), color="darkred") +
    # geom_ribbon(aes(x=year_offsetted, ymin = ratio_yoy_lwr, ymax = ratio_yoy_upr), fill="darkred", alpha=0.2) +
    facet_wrap(~SOURCETY) +
    rcrea::theme_crea() +
    scale_x_continuous(breaks=seq(2000, 2020, 2), expand=c(0,0)) +
    scale_y_continuous(labels=scales::percent) +
    geom_hline(yintercept=0) +
    labs(y="SO2 emission [kton/yr]", x=NULL,
         title="Year-on-year variation of SO2 emissions per sector",
         subtitle="Estimations for the August-to-July periods across 588 identified hotspots",
         caption="Source: CREA estimation based on NASA OMI and MEaSUREs.")
}
