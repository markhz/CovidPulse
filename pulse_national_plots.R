
rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts"
setwd(path_wd)

library(tidyverse)
library(egg)
library(GGally)
library(RColorBrewer)
library(lubridate)
source("pulse_process_functions.R")
source("pulse_plot_functions.R")

# -------------------------------------------------------------------------


# data path
dataPath <- "../Data/National_PulseData_Paper1"
refDataPath <- file.path(dataPath, "..")

regionLevels <- c("Full Sample", 
                  "Northeast",
                  "South",
                  "Midwest",
                  "West")
regionLevels_rename <- c("United States", 
                  "Northeast",
                  "South",
                  "Midwest",
                  "West")

df_covid_summ <- read.csv(file.path(dataPath, "covid_region_summary.csv"))%>%
  mutate(REGION = factor(REGION, levels = regionLevels, labels = regionLevels_rename  ),
         date = as.Date(date) )
df_covid_summ_us <-  df_covid_summ %>% 
  filter(REGION == "United States") %>%
  select( c("date", "cases_per_mean", "deaths_per_mean", "d_cases_per_mean") )


# csvPrefix <- "National_Pulse_pweightPrevalence_"
# pngPrefix <- "TimeSeries_Pulse_pweightPrevalence_"  
# csvPrefix <- "National_Pulse_pweightPrevImputed_"
# pngPrefix <- "TimeSeries_Pulse_pweightPrevImputed_"
csvPrefix <- "National_Pulse_pweightPrevImputed_workingage_"
pngPrefix <- "TimeSeries_Pulse_pweightPrevImputed_workingage_"
# csvPrefix <- "National_Pulse_pweightPrevImputed_REGION1_"
# pngPrefix <- "TimeSeries_Pulse_pweightPrevImputed_REGION1_"

prevFilePath_region <- file.path(dataPath, paste0(csvPrefix, "byREGION.csv")) 
prevFilePath_gender <- file.path(dataPath, paste0(csvPrefix, "byGENDER.csv"))
prevFilePath_race2 <- file.path(dataPath, paste0(csvPrefix, "byRACE_2.csv"))
prevFilePath_race5 <- file.path(dataPath, paste0(csvPrefix, "byRACE_5.csv"))
prevFilePath_edu2 <- file.path(dataPath, paste0(csvPrefix, "byEDU_2.csv"))
prevFilePath_age3 <- file.path(dataPath, paste0(csvPrefix, "byAGE_3.csv"))
prevFilePath_age4 <- file.path(dataPath, paste0(csvPrefix, "byAGE_4.csv"))
# prevFilePath_income4 <- file.path(dataPath, paste0(csvPrefix, "byINCOME_4.csv"))
prevFilePath_lostwork <- file.path(dataPath, paste0(csvPrefix, "byLOST_WORK.csv"))
prevFilePath_nowork <- file.path(dataPath, paste0(csvPrefix, "byNOWORK_COVID.csv"))
prevFilePath_state <- file.path(dataPath, paste0(csvPrefix, "bySTATE.csv"))

covidFilePath <- file.path(refDataPath, "CovidTracking", "covidtracking_data.csv")

outputFigPath <- file.path(dataPath, "Figures")
dir.create(outputFigPath, showWarnings = FALSE)

figPath_region_comp <- file.path(outputFigPath, paste0(pngPrefix, "byREGION_compare.png"))
figPath_gender_comp <- file.path(outputFigPath, paste0(pngPrefix, "byGENDER_compare.png"))
figPath_race2_comp <- file.path(outputFigPath, paste0(pngPrefix, "byRACE_2_compare.png"))
figPath_race5_comp <- file.path(outputFigPath, paste0(pngPrefix, "byRACE_5_compare.png"))
figPath_edu2_comp <- file.path(outputFigPath, paste0(pngPrefix, "byEDU_2_compare.png"))
figPath_age3_comp <- file.path(outputFigPath, paste0(pngPrefix, "byAGE_3_compare.png"))
figPath_age4_comp <- file.path(outputFigPath, paste0(pngPrefix, "byAGE_4_compare.png"))
# figPath_income4_comp <- file.path(outputFigPath, paste0(pngPrefix, "byINCOME_4_compare.png"))
figPath_lostwork_comp <- file.path(outputFigPath, paste0(pngPrefix, "byLOST_WORK_compare.png"))
figPath_nowork_comp <- file.path(outputFigPath, paste0(pngPrefix, "byNOWORK_COVID_compare.png"))
figPath_state_comp <- file.path(outputFigPath, paste0(pngPrefix, "bySTATE_compare.png"))

figPathBase_region_covid <- file.path(outputFigPath, paste0(pngPrefix, "byREGION_covidcompare_"))


# outcomeLevels <- c("Severe PHQ-4 Score (>= 9)",
#                    "High GAD-2 Score (>= 5)",
#                    "Two Symptoms of Anxiety",
#                    "High PHQ-2 Score (>= 5)",
#                    "Two Symptoms of Depression")
# outcomeLevels_rename <- c("Severe PHQ-4 Score (\u2265 9)" ,
#                          # "High GAD-2 Score (\u2265 5)",
#                          "Anxiety Symptoms",
#                          "Two Symptoms of Anxiety",
#                          # "High PHQ-2 Score (\u2265 5)",
#                          "Depression Symptoms",
#                          "Two Symptoms of Depression")
outcomeLevels <- c("High GAD-2 Score (>= 5)",
                   "High PHQ-2 Score (>= 5)")
outcomeLevels_rename <- c("Anxiety Symptoms\n(GAD-2 \u2265 5)",
                          "Depression Symptoms\n(PHQ-2 \u2265 5)")
outcomeLevelsPlot <- outcomeLevels_rename



# genderLevels <- c("Female",
#                   "Male")
# 
# race2Levels <- c("Non-white",
#                  "White")
# 
# race5Levels <- c("Non-hispanic White",
#                  "Non-hispanic Black",
#                  "Hispanic",
#                  "Non-hispanic Asian",
#                  "Other or multiple")
# 
# edu2Levels <- c("< College",
#                 ">= College")
# 
# 
# age3Levels <- c("18 - 39",
#                 "40 - 64",
#                 ">= 65")
# 
# age4Levels <- c("18 - 24",
#                 "25 - 44",
#                 "45 - 64",
#                 ">= 65")
# 
# income4Levels <- c("0 - 34,999",
#                    "35,000 - 74,999",
#                    "75,000 - 149,999",
#                    "> 150,000")
# 
# lostworkLevels <- c("Household lost employment income", 
#                     "Household did not lose employment income")



# fig_comp_width <- 20
fig_comp_width <- 12
fig_comp_height <- 10
fig_covid_width <- 6.5
fig_covid_height <- 12


# -------------------------------------------------------------------------
# load tables
# -------------------------------------------------------------------------

# df_prev_region <- import_clean_df_prev(prevFilePath_region, stratVar = "REGION", outcomeLevels) %>%
#   mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) )
df_prev_region <- import_clean_df_prev(prevFilePath_region, stratVar = "REGION", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename)) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_gender <- import_clean_df_prev(prevFilePath_gender, stratVar = "GENDER", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_race2 <- import_clean_df_prev(prevFilePath_race2, stratVar = "RACE_2", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_race5 <- import_clean_df_prev(prevFilePath_race5, stratVar = "RACE_5", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_edu2 <- import_clean_df_prev(prevFilePath_edu2, stratVar = "EDU_2", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_age3 <- import_clean_df_prev(prevFilePath_age3, stratVar = "AGE_3", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_age4 <- import_clean_df_prev(prevFilePath_age4, stratVar = "AGE_4", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_lostwork <- import_clean_df_prev(prevFilePath_lostwork, stratVar = "LOST_WORK", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
# df_prev_state <- import_clean_df_prev(prevFilePath_state, stratVar = "STATE", outcomeLevels) %>%
#   mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
#   left_join(df_covid_summ_us, by = c("startDate" = "date"))

df_prev_nowork <- import_clean_df_prev(prevFilePath_nowork, stratVar = "NOWORK_COVID", outcomeLevels) %>%
  mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) ) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))




# -------------------------------------------------------------------------
# plot
# -------------------------------------------------------------------------

# plot correlations (full sample; employment income loss)
# -------------------------------------------------------------------------





# pulse compare across strata and outcome
# -------------------------------------------------------------------------


# save_stratPlot(df_prev_region, "REGION", figPath_region_comp, outcomePlot = outcomeLevelsPlot)

save_stratPlot(df_prev_gender, "GENDER", figPath_gender_comp, outcomePlot = outcomeLevelsPlot)

save_stratPlot(df_prev_race2, "RACE_2", figPath_race2_comp, outcomePlot = outcomeLevelsPlot)

save_stratPlot(df_prev_race5, "RACE_5", figPath_race5_comp, outcomePlot = outcomeLevelsPlot)

save_stratPlot(df_prev_edu2, "EDU_2", figPath_edu2_comp, outcomePlot = outcomeLevelsPlot)

save_stratPlot(df_prev_age3, "AGE_3", figPath_age3_comp, outcomePlot = outcomeLevelsPlot)

save_stratPlot(df_prev_age4, "AGE_4", figPath_age4_comp, outcomePlot = outcomeLevelsPlot)

save_stratPlot(df_prev_lostwork, "LOST_WORK", figPath_lostwork_comp, outcomePlot = outcomeLevelsPlot)

# save_stratPlot(df_prev_state, "STATE", figPath_state_comp, outcomePlot = outcomeLevelsPlot)


save_stratPlot(df_prev_nowork, "NOWORK_COVID", figPath_nowork_comp, outcomePlot = outcomeLevelsPlot)

# save_stratPlot(df_prev_edu4, "EDU_4", figPath_edu4_comp, outcomePlot = outcomeLevelsPlot)

# save_stratPlot(df_prev_income4, "INCOME_4", figPath_income4_comp, outcomePlot = outcomeLevelsPlot)



# -------------------------------------------------------------------------
# covid 
# -------------------------------------------------------------------------
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73","#CC79A7", "#F0E442", "#0072B2", "#D55E00")



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

t_nosurvey <- c(max(df_prev_region$endDate[df_prev_region$Phase == 1]), min(df_prev_region$startDate[df_prev_region$Phase == 2]))
xlimits <- c(min(df_prev_region$startDate), max(df_prev_region$endDate))
font_size <- 20

# covid case plot
p_cases_us <- df_covid_summ %>%
  filter(REGION == "United States") %>%
  filter( date >= min(df_prev_region$startDate) &
            date <= max(df_prev_region$endDate))  %>%
  ggplot( aes(x = date, y = cases_per_mean ), col = "black" ) +
  
  geom_rect(
    fill = "lightgray",
    col = "lightgray",
    alpha = 0.7, 
    xmin = t_nosurvey[1],
    xmax = t_nosurvey[2],
    ymin = -Inf,
    ymax = Inf
  ) +
  
  geom_line(size = 2) +
  
  
  theme_bw() +
  ylim(0, NA) +
  ylab("New Daily Cases per Million\n(7-day Average)")  +
  xlab("") +
  
  scale_color_manual(values=cbbPalette) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b",
               limits = xlimits) +
  theme(legend.position = "none",
        text = element_text(size = font_size),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# -------------------------------------------------------------------------
outcome_plot <- outcomeLevelsPlot
outcome_file_labels <- c("GAD2", "PHQ2")



# plot prevalence full sample
p_prev_us <- df_prev_region %>%
  filter( Outcome %in% outcome_plot & REGION == "United States") %>%
  # left_join(df_corr_region, by = c("REGION", "Outcome", "PHASE2")) %>%
  
  plot_timeseries_prev("Outcome") +
  geom_rect(
    fill = "lightgray",
    col = "lightgray",
    alpha = 0.7, 
    # xmin = decimal_date(as.Date(c("1924-01-01"))),
    # xmax = decimal_date(as.Date(c("1928-12-31"))),
    xmin = t_nosurvey[1],
    xmax = t_nosurvey[2],
    ymin = -Inf,
    ymax = Inf
  ) +
  # scale_color_manual(values=rev(cbbPalette)) +
  scale_color_manual(values=cbbPalette[2:length(cbbPalette)]) +
  theme(legend.position = "top",
        text = element_text(size = font_size),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  # xlab("") +
  ylab("Prevalance % (95% CI)") +
  xlab("") +
  ylim(0, NA) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b",
               limits = xlimits) +
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.justification = "left",
    legend.key.width = unit(3, "line"),
    legend.margin = margin(0,.25,0,0, unit="cm"),
    text = element_text(size = font_size)
  ) +
  guides(col=guide_legend(ncol=1, order = 2), 
         shape=guide_legend(ncol=1, order = 1) ) 




p_prev_covid_us <- ggarrange(p_cases_us, p_prev_us, ncol = 1,
                             heights = c(1,1.5))

ggsave(paste0(figPathBase_region_covid, "National_GAD2PHQ2_Covid.png") ,
       plot = p_prev_covid_us,
       width = fig_covid_width,
       height = fig_covid_height,
       dpi = 300,
       units = "in",
       device='png')




# -------------------------------------------------------------------------
# correlation coefficient with demographic variables
# -------------------------------------------------------------------------

# getcoef <- function(df){
#   m <- coef( lm(prop ~ cases_per_mean, data = df) )[[2]]
#   return(m)
# }
# 
# df_prev_region%>%group_by(Outcome)%>%getcoef()
# 
# sym_out<-sym("prop")
# a<-df_prev_lostwork %>% 
#   group_by(Outcome, PHASE2, LOST_WORK) %>%
#   do( tidy( lm(!!sym_out ~ cases_per_mean, data = .) ) ) %>%
#   filter(term == "cases_per_mean") %>%
#   mutate(ci_l = estimate - (1.96 * std.error),
#          ci_u = estimate + (1.96 * std.error) ) %>%
#   select(c("Outcome", "PHASE2", "LOST_WORK", "estimate", "ci_l", "ci_u", "p.value") ) %>%
#   
#   rename(m_abs = estimate,
#          m_ci_l_abs = ci_l,
#          m_ci_u_abs = ci_u,
#          pval_abs = p.value,
#          stratVar = LOST_WORK) 
# 
# sym_out<-sym("pctchg_prop")
# b<-df_prev_lostwork %>% 
#   group_by(Outcome, PHASE2, LOST_WORK) %>%
#   do( tidy( lm(!!sym_out ~ cases_per_mean, data = .) ) ) %>%
#   filter(term == "cases_per_mean") %>%
#   mutate(ci_l = estimate - (1.96 * std.error),
#          ci_u = estimate + (1.96 * std.error) ) %>%
#   select(c("Outcome", "PHASE2", "LOST_WORK", "estimate", "ci_l", "ci_u", "p.value") ) %>%
#   
#   rename(m_abs = estimate,
#          m_ci_l_abs = ci_l,
#          m_ci_u_abs = ci_u,
#          pval_abs = p.value,
#          stratVar = LOST_WORK) 
# 
# a %>%
#   filter(PHASE2 == "Phase 1") %>%
#   ggplot() + 
#   geom_pointrange(aes(x = stratVar, y = m_abs, ymin = m_ci_l_abs, ymax = m_ci_u_abs, col = stratVar)) +
#   facet_wrap(vars(Outcome), ncol = 1)
# a %>%
#   filter(PHASE2 == "Phase 2 & 3") %>%
#   ggplot() + 
#   geom_pointrange(aes(x = stratVar, y = m_abs, ymin = m_ci_l_abs, ymax = m_ci_u_abs, col = stratVar)) +
#   facet_wrap(vars(Outcome), ncol = 1)
# 
# b %>%
#   filter(PHASE2 == "Phase 1") %>%
#   ggplot() + 
#   geom_pointrange(aes(x = stratVar, y = m_abs, ymin = m_ci_l_abs, ymax = m_ci_u_abs, col = stratVar)) +
#   facet_wrap(vars(Outcome), ncol = 1)
# b %>%
#   filter(PHASE2 == "Phase 2 & 3") %>%
#   ggplot() + 
#   geom_pointrange(aes(x = stratVar, y = m_abs, ymin = m_ci_l_abs, ymax = m_ci_u_abs, col = stratVar)) +
#   facet_wrap(vars(Outcome), ncol = 1)
# 
# 
# 
# 
# df_prev_region %>%filter(REGION == "United States") %>%
# ggplot(aes(x=cases_per_mean,y=prop, group = PHASE2, col = PHASE2)) + 
#   geom_point()+
#   geom_smooth(method=lm) +
#   facet_wrap(vars(Outcome), nrow = 1)
# 
# 
# 
# 
# # c
# df_prev_lostwork %>%filter(PHASE2 == "Phase 1") %>%
#   ggplot(aes(x=cases_per_mean,y=prop, group = LOST_WORK, col = LOST_WORK)) + 
#   geom_point()+
#   geom_smooth(method=lm, se = FALSE) +
#   facet_wrap(vars(Outcome), nrow = 1) +
#   theme(legend.position = "top")
# df_prev_lostwork %>%filter(PHASE2 == "Phase 2 & 3") %>%
#   ggplot(aes(x=cases_per_mean,y=prop, group = LOST_WORK, col = LOST_WORK)) + 
#   geom_point()+
#   geom_smooth(method=lm, se = FALSE) +
#   facet_wrap(vars(Outcome), nrow = 1) +
#   theme(legend.position = "top")
# df_prev_lostwork %>%filter(PHASE2 == "Phase 1") %>%
#   ggplot(aes(x=cases_per_mean,y=pctchg_prop, group = LOST_WORK, col = LOST_WORK)) + 
#   geom_point()+
#   geom_smooth(method=lm, se = FALSE) +
#   facet_wrap(vars(Outcome), nrow = 1) +
#   theme(legend.position = "top")
# df_prev_lostwork %>%filter(PHASE2 == "Phase 2 & 3") %>%
#   ggplot(aes(x=cases_per_mean,y=pctchg_prop, group = LOST_WORK, col = LOST_WORK)) + 
#   geom_point()+
#   geom_smooth(method=lm, se = FALSE) +
#   facet_wrap(vars(Outcome), nrow = 1) +
#   theme(legend.position = "top")

# -------------------------------------------------------------------------




# -------------------------------------------------------------------------
# covid trend comparison
# -------------------------------------------------------------------------




# correlation to cases

df_prev_region_corr <- df_prev_region %>%
  rename(cases_per_mean_US = cases_per_mean) %>%
  left_join(df_covid_summ, by = c("REGION", "startDate" = "date")) %>%
  filter(Outcome %in% outcomeLevelsPlot) %>%
  arrange(Outcome, REGION, WEEK)

repCasesUS <- df_prev_region_corr$cases_per_mean[df_prev_region_corr$REGION == "United States"]
# df_prev_region_corr$cases_per_mean_US <- rep(repCasesUS,5)

df_corr_region <- df_prev_region_corr %>%
  group_by(REGION, Outcome, PHASE2) %>%
  summarize(corr_intraregion= round( cor(prop, cases_per_mean, method = "pearson"), digits = 2),
            corr_US= round(cor(prop, cases_per_mean_US, method = "pearson"), digits = 2),
            .groups = 'drop')

df_prev_region_corr %>%
  filter(REGION %in% levels(REGION)[1],
         Outcome == outcomeLevelsPlot[1]) %>%
ggpairs(aes( col = PHASE2),
        col = c("prop", "cases_per_mean")) 


df_prev_region_corr %>%
  filter(REGION %in% levels(REGION)[1],
         Outcome == outcomeLevelsPlot[2]) %>%
  ggpairs(aes( col = PHASE2),
          col = c("prop", "cases_per_mean")) 

df_prev_lostwork %>%
  filter(Outcome == outcomeLevelsPlot[1]) %>%
  ggpairs(aes( col = LOST_WORK),
          col = c("prop", "cases_per_mean")) 

df_prev_lostwork %>%
  filter(Outcome == outcomeLevelsPlot[2]) %>%
  ggpairs(aes( col = LOST_WORK),
          col = c("prop", "cases_per_mean")) 


df_prev_edu2 %>%
  filter(Outcome == outcomeLevelsPlot[1]) %>%
  ggpairs(aes( col = EDU_2),
          col = c("prop", "cases_per_mean")) 

df_prev_edu2 %>%
  filter(Outcome == outcomeLevelsPlot[2]) %>%
  ggpairs(aes( col = EDU_2),
          col = c("prop", "cases_per_mean")) 




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# regional covid

p_cases <- df_covid_summ %>%
  filter( date >= min(df_prev_region$startDate) &
            date <= max(df_prev_region$endDate))  %>%
  filter (REGION != "United States") %>%
  ggplot( aes(x = date, y = cases_per_mean , col = REGION) ) +
  geom_rect(
    fill = "lightgray",
    col = "lightgray",
    alpha = 0.7, 
    # xmin = decimal_date(as.Date(c("1924-01-01"))),
    # xmax = decimal_date(as.Date(c("1928-12-31"))),
    xmin = t_nosurvey[1],
    xmax = t_nosurvey[2],
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line(size = 2) +
  theme_bw() +
  # scale_color_manual(values=cbbPalette) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  # facet_wrap(vars(REGION), nrow = 1) +
  ylab("New Daily Cases per Million\n(7-day Average)") +
  xlab("")

p_cases



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
  
  
  x_dodge = position_dodge(width=2 )
  
  stratVarSym <- as.symbol("REGION")
  # plot prevalence by region
  p_prev <- df_prev_region %>%
    filter( Outcome == outcome_plot,
            REGION != "United States") %>%
    left_join(df_corr_region, by = c("REGION", "Outcome", "PHASE2")) %>%
    
    # plot_timeseries_prev("REGION")
  
  

  
  
    mutate(Phase = factor(Phase, 
                          levels = c(1,2,3),
                          labels = c("Phase 1", "Phase 2", "Phase 3") ) ) %>%
    
    ggplot( aes(x = midDate, 
                y = prop, 
                # group = interaction(!!stratVarSym, Phase), 
                group = interaction(!!stratVarSym, PHASE2), 
                col = !!stratVarSym ) ) +
    geom_rect(
      fill = "lightgray",
      col = "lightgray",
      alpha = 0.7, 
      # xmin = decimal_date(as.Date(c("1924-01-01"))),
      # xmax = decimal_date(as.Date(c("1928-12-31"))),
      xmin = t_nosurvey[1],
      xmax = t_nosurvey[2],
      ymin = -Inf,
      ymax = Inf
    ) +
    geom_line(size = 1.5,
              position = x_dodge) +
    # geom_point(#aes(shape = Phase ) , 
    #   aes(shape = PHASE2 ) , 
    #   size = 3,
    #   position = x_dodge) +
    
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    scale_x_date(date_breaks = "2 months",
                 date_labels = "%b")+
  
  
  
    # geom_text(data=df_text, 
    #           aes(x, y, label=lab),
    #           hjust = 0) +
    # # scale_color_manual(values=cbbPalette) +
    # facet_wrap(vars(REGION), nrow = 1) +
    theme(legend.title =  element_blank(),
          legend.position = "top",
          text = element_text(size = 24),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("") +
    ylab(paste0(outcome_plot, "\nPrevalance % (95% CI)")) +
    ylim(0, NA) +
    guides(col=guide_legend(ncol=2))
    # ylim(0, 22.5)
  
  
  p_prev_covid_region <- ggarrange(p_cases, p_prev, ncol = 1, 
                            heights = c(1,1.5))
  
  ggsave(paste0(figPathBase_region_covid, outcome_plot_file_label, ".png") ,
         plot = p_prev_covid_region,
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

