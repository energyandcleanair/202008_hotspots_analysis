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
