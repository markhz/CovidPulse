

plot_barplot_prev <- function(df_plot, stratVar){
  stratSym <- as.symbol(stratVar)
  
  df_plot %>%
    ggplot(aes(x = !!stratSym, y = prop)) +
    geom_bar(stat = "identity",
             width = 0.5,
             fill = "lightblue",
             col = "black") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_u) ,
                  width = 0.2) + 
    theme_bw() +
    # facet_wrap(vars(Outcome), ncol = 1) +
    facet_wrap(vars(Outcome), nrow = 1) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) + 
    coord_flip() +
    ylab(NULL) 

  
}

plot_timeseries_prev <- function(df_plot, stratVar){
  
  x_dodge = position_dodge(width=0 )
  # x_dodge = position_dodge(width=2 )
  
  stratVarSym <- as.symbol(stratVar)
  
  
  p1 <- df_plot %>%
    mutate(Phase = factor(Phase, 
                          levels = c(1,2,3),
                          labels = c("Phase 1", "Phase 2", "Phase 3") ) ) %>%
    
    ggplot( aes(x = midDate, 
                y = prop, 
                # group = interaction(!!stratVarSym, Phase), 
                group = interaction(!!stratVarSym, PHASE2), 
                col = !!stratVarSym ) ) +
    geom_line(size = 1.5,
              position = x_dodge) +
    geom_point(#aes(shape = Phase ) , 
               aes(shape = PHASE2 ) , 
               size = 3,
               position = x_dodge) +
    # geom_errorbar(aes(ymin = ci_l, ymax = ci_u), 
    #               width = 2,
    #               size = .5,
    #               position = x_dodge)  +
    geom_ribbon(aes(ymin = ci_l, ymax = ci_u, fill = !!stratVarSym), 
                col = NA, 
                alpha = 0.2) +
    
    theme_bw() +
    scale_color_brewer(palette = "Dark2", direction=-1) +
    scale_fill_brewer(palette = "Dark2", direction=-1, guide = FALSE) +
    scale_x_date(date_breaks = "2 months",
                 date_labels = "%b")
  
  return(p1)
}





plot_stratvar_comparison <- function(df_plot,stratVar, facet_strat = FALSE, plot_full = FALSE){
  
  stratVarSym <- as.symbol(stratVar)
  
  # two separate data frames for different plot styles
  df_plot_strat <- df_plot[ df_plot[[stratVar]] != "Full Sample", ] 
  
  
  p1 <- df_plot_strat %>%
    plot_timeseries_prev(stratVar) + 
    
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.justification = "left",
      legend.key.width = unit(5, "line"),
      # legend.spacing.x = unit(0.5, "in"),
      legend.margin = margin(0,2,0,0, unit="cm"),
      panel.spacing = unit(5, "lines"),
      text = element_text(size = 28)
    ) +
    guides(col=guide_legend(ncol=1, order = 2), 
           shape=guide_legend(ncol=1, order = 1) ) +
    xlab("") +
    ylab("Prevalance % (95% CI)") 
  
  if (facet_strat){
    p1 <- p1 + facet_grid(col = vars(Outcome), row = vars(!!stratVarSym) )
  }
  else{
    p1 <- p1 + facet_wrap(vars(Outcome), nrow = 1)
  }
  
  if (plot_full){
    
    df_plot_full <- df_plot[ df_plot[[stratVar]] == "Full Sample", ] 
    
    p1 <- p1 + 
      # overlay time series of full sample in bold
      geom_line(data = df_plot_full, aes(x = midDate, y = prop, group = !!stratVarSym),
                col = "black", size = 2) +
      # geom_point(data = df_plot_full, aes(x = midDate, y = prop, shape = Phase ),
      geom_point(data = df_plot_full, aes(x = midDate, y = prop, shape = PHASE2 ),
                 col = "black", size = 3.5) +
      geom_errorbar(data = df_plot_full, aes(x = midDate, ymin = ci_l, ymax = ci_u),
                    col = "black",
                    width = .3,
                    size = 1.25)  
  }
  
  
  return(p1)
}





save_stratPlot <- function(df_plot, 
                           stratVar, 
                           figOutputPath, 
                           outcomePlot = levels(df_plot$Outcome),
                           fig_comp_width = 16,
                           fig_comp_height = 10) {
  p1 <- df_plot %>%
    filter(Outcome %in% outcomePlot) %>%
    plot_stratvar_comparison(stratVar = stratVar, plot_full = FALSE) +
    ylim(0, NA)
    # ylim(0, 30) 
  
  ggsave( figOutputPath ,
          plot = p1,
          width = fig_comp_width,
          height = fig_comp_height,
          dpi = 300, 
          units = "in", 
          device='png')
  
  return(p1)
}





# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# placeholder to save code for regional covid comparison plots
placeholder_region_covid_plot <- function(){
  # covid case plot
  p_cases <- df_covid_summ %>%
    filter( date >= min(df_prev_region$startDate) &
              date <= max(df_prev_region$endDate))  %>%
    ggplot( aes(x = date, y = cases_per_mean , col = REGION) ) +
    geom_line(size = 2) +
    theme_bw() +
    scale_y_continuous(trans='log10',
                       breaks = c(10, 100, 1000),
                       limits = c(10, 1000)) +
    scale_color_manual(values=cbbPalette) +
    scale_x_date(date_breaks = "2 months",
                 date_labels = "%b") +
    theme(legend.position = "none",
          text = element_text(size = 24),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(vars(REGION), nrow = 1) +
    ylab("New Daily Cases per Million\n(7-day Average; Log Scale)") +
    xlab("")
  
  p_cases
  
  
  # covid mortality plot
  p_deaths <- df_covid_summ %>%
    filter( date >= min(df_prev_region$startDate) &
              date <= max(df_prev_region$endDate))  %>%
    # ggplot( aes(x = date, y = log10(deaths_per_mean) , col = REGION) ) +
    ggplot( aes(x = date, y = deaths_per_mean , col = REGION) ) +
    geom_line(size = 2) +
    theme_bw() +
    scale_y_continuous(trans='log10',
                       breaks = c(.1, 1, 10, 100),
                       limits = c(.1, 100)) +
    scale_color_manual(values=cbbPalette) +
    scale_x_date(date_breaks = "2 months",
                 date_labels = "%b") +
    theme(legend.position = "none",
          text = element_text(size = 24))+
    facet_wrap(vars(REGION), nrow = 1) +
    ylab("14-day Percent Change in Covid-19 Mortality (%)") +
    # ylab("log [ 7-Day Average of \nCovid-19 Deaths (per 1,000,000) ]") +
    xlab("")
  p_deaths
  # covid comparison plot
  # -------------------------------------------------------------------------
  
  # population text data...
  text_x <- as.Date("2020-05-01")
  text_y <- 5
  text1 <- "Correlation coefficient with\n"
  text_us <- "Covid-19 trends in United States: "
  
  
  # outcome_file_labels <- c("PHQ4", "GAD2", "Anxiety2", "PHQ2", "Depression2")
  outcome_file_labels <- c("GAD2", "PHQ2")
  
  for ( i in 1:length(outcome_file_labels) ){
    
    outcome_plot_file_label <- outcome_file_labels[i]
    outcome_plot <- outcomeLevelsPlot[i]
    
    
    text_label <- c( paste0(text1,text_us, 
                            format(df_corr_region$corr_US[df_corr_region$REGION == "United States" & df_corr_region$Outcome == outcome_plot], nsmall = 2)),
                     paste0(text1, "Covid-19 trends in Northeast: ", 
                            format( df_corr_region$corr_intraregion[df_corr_region$REGION == "Northeast" & df_corr_region$Outcome == outcome_plot], nsmall = 2),
                            "\n", text_us,
                            format(df_corr_region$corr_US[df_corr_region$REGION == "Northeast" & df_corr_region$Outcome == outcome_plot], nsmall = 2)),
                     paste0(text1, "Covid-19 trends in South: ", 
                            format(df_corr_region$corr_intraregion[df_corr_region$REGION == "South" & df_corr_region$Outcome == outcome_plot], nsmall = 2),
                            "\n", text_us,
                            format(df_corr_region$corr_US[df_corr_region$REGION == "South" & df_corr_region$Outcome == outcome_plot], nsmall = 2)),
                     paste0(text1, "Covid-19 trends in Midwest: ", 
                            format(df_corr_region$corr_intraregion[df_corr_region$REGION == "Midwest" & df_corr_region$Outcome == outcome_plot], nsmall = 2),
                            "\n", text_us,
                            format(df_corr_region$corr_US[df_corr_region$REGION == "Midwest" & df_corr_region$Outcome == outcome_plot], nsmall = 2)),
                     paste0(text1, "Covid-19 trends in West: ", 
                            format(df_corr_region$corr_intraregion[df_corr_region$REGION == "West" & df_corr_region$Outcome == outcome_plot], nsmall = 2),
                            "\n", text_us,
                            format(df_corr_region$corr_US[df_corr_region$REGION == "West" & df_corr_region$Outcome == outcome_plot], nsmall = 2)))
    text_region <- regionLevels_rename
    df_text <- data.frame(x = text_x, y = text_y, lab = text_label, REGION = text_region, Phase = NA)
    
    
    # plot prevalence by region
    p_prev <- df_prev_region %>%
      filter( Outcome == outcome_plot) %>%
      left_join(df_corr_region, by = c("REGION", "Outcome")) %>%
      
      plot_timeseries_prev("REGION") +
      geom_text(data=df_text, 
                aes(x, y, label=lab),
                hjust = 0) +
      scale_color_manual(values=cbbPalette) +
      facet_wrap(vars(REGION), nrow = 1) +
      theme(legend.position = "none",
            text = element_text(size = 24),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      xlab("") +
      ylab(paste0(outcome_plot, "\nPrevalance % (95% CI)")) +
      # ylim(0, NA)
      ylim(0, 22.5)
    
    
    p_prev_covid <- ggarrange(p_cases, p_prev, ncol = 1)
    
    ggsave(paste0(figPathBase_region_covid, outcome_plot_file_label, ".png") ,
           plot = p_prev_covid,
           width = fig_covid_width,
           height = fig_covid_height,
           dpi = 300,
           units = "in",
           device='png')
    
    
    # p_prev_covid_deaths <- ggarrange(p_deaths, p_prev, ncol = 1)
    
    # ggsave(paste0(figPathBase_region_covid, "deaths_", outcome_plot_file_label, ".png") ,
    #        plot = p_prev_covid_deaths,
    #        width = fig_covid_width,
    #        height = fig_covid_height,
    #        dpi = 300,
    #        units = "in",
    #        device='png')
  }


}


