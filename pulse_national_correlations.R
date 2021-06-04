
rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts"
setwd(path_wd)

library(tidyverse)
library(egg)
library(GGally)
library(RColorBrewer)
library(lubridate)
library(broom)
source("pulse_process_functions.R")
source("pulse_plot_functions.R")

# -------------------------------------------------------------------------

# csvPrefix <- "National_Pulse_pweightPrevalence_"
# csvPrefix <- "National_Pulse_pweightPrevImputed_"
csvPrefix <- "National_Pulse_pweightPrevImputed_workingage_"


# data path
dataPath <- "../Data/National_PulseData_Paper1"
refDataPath <- file.path(dataPath, "..")





# -------------------------------------------------------------------------



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


outcomeLevels <- c("High GAD-2 Score (>= 5)",
                   "High PHQ-2 Score (>= 5)")
outcomeLevels_rename <- c("Anxiety Symptoms\n(GAD-2 \u2265 5)",
                          "Depression Symptoms\n(PHQ-2 \u2265 5)")
outcomeLevelsPlot <- outcomeLevels_rename


prevFilePath_region <- file.path(dataPath, paste0(csvPrefix, "byREGION.csv")) 
prevFilePath_gender <- file.path(dataPath, paste0(csvPrefix, "byGENDER.csv"))
prevFilePath_race2 <- file.path(dataPath, paste0(csvPrefix, "byRACE_2.csv"))
prevFilePath_race5 <- file.path(dataPath, paste0(csvPrefix, "byRACE_5.csv"))
prevFilePath_edu2 <- file.path(dataPath, paste0(csvPrefix, "byEDU_2.csv"))
prevFilePath_age3 <- file.path(dataPath, paste0(csvPrefix, "byAGE_3.csv"))
prevFilePath_income4 <- file.path(dataPath, paste0(csvPrefix, "byINCOME_4.csv"))
prevFilePath_lostwork <- file.path(dataPath, paste0(csvPrefix, "byLOST_WORK.csv"))
prevFilePath_nowork <- file.path(dataPath, paste0(csvPrefix, "byNOWORK_COVID.csv"))
covidFilePath <- file.path(refDataPath, "CovidTracking", "covidtracking_data.csv")
# -------------------------------------------------------------------------


df_covid_summ <- read.csv(file.path(dataPath, "covid_region_summary.csv"))%>%
  mutate(REGION = factor(REGION, levels = regionLevels, labels = regionLevels_rename  ),
         date = as.Date(date) )
df_covid_summ_us <-  df_covid_summ %>% 
  filter(REGION == "United States") %>%
  select( c("date", "cases_per_mean", "deaths_per_mean", "d_cases_per_mean") )





# -------------------------------------------------------------------------
# load tables
# -------------------------------------------------------------------------

df_prev_region <- import_clean_df_prev(prevFilePath_region, stratVar = "REGION", outcomeLevels) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_gender <- import_clean_df_prev(prevFilePath_gender, stratVar = "GENDER", outcomeLevels) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_race2 <- import_clean_df_prev(prevFilePath_race2, stratVar = "RACE_2", outcomeLevels) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_race5 <- import_clean_df_prev(prevFilePath_race5, stratVar = "RACE_5", outcomeLevels) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_edu2 <- import_clean_df_prev(prevFilePath_edu2, stratVar = "EDU_2", outcomeLevels) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_age3 <- import_clean_df_prev(prevFilePath_age3, stratVar = "AGE_3", outcomeLevels) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_lostwork <- import_clean_df_prev(prevFilePath_lostwork, stratVar = "LOST_WORK", outcomeLevels) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))
df_prev_nowork <- import_clean_df_prev(prevFilePath_nowork, stratVar = "NOWORK_COVID", outcomeLevels) %>%
  left_join(df_covid_summ_us, by = c("startDate" = "date"))

# -------------------------------------------------------------------------
# correlation coefficient with demographic variables
# -------------------------------------------------------------------------





get_lm_coef <- function(df, metricStr, stratVar){
  df_out<-df %>% 
    group_by(Outcome, PHASE2, !!sym(stratVar)) %>%
    do( tidy( lm(!!sym(metricStr) ~ cases_per_mean, data = .) ) ) %>%
    filter(term == "cases_per_mean") %>%
    mutate(ci_l = estimate - (1.96 * std.error),
           ci_u = estimate + (1.96 * std.error)) %>%
    mutate(str_out = sprintf("%.4f (%.4f, %.4f)",
                         round(estimate,4), 
                         round(ci_l, 4),
                         round(ci_u, 4)    ) ) %>%
    select(c("Outcome", "PHASE2", stratVar, "estimate", "ci_l", "ci_u", "p.value", "str_out") ) %>%
    rename(stratVar = !!sym(stratVar))
  
  return(df_out)
}





get_lm_metrics <- function(df_prev, stratVar){
  df_slope_abs <- df_prev %>% get_lm_coef("prop", stratVar)%>%
    rename(m_abs = estimate,
           m_ci_l_abs = ci_l,
           m_ci_u_abs = ci_u,
           pval_abs = p.value,
           str_abs = str_out) 
  df_slope_rel <- df_prev %>% get_lm_coef("pctchg_prop", stratVar) %>%
    rename(m_rel = estimate,
           m_ci_l_rel = ci_l,
           m_ci_u_rel = ci_u,
           pval_rel = p.value,
           str_rel = str_out) 
  
  df_slope <- df_slope_abs %>%
    left_join(df_slope_rel, c("Outcome", "PHASE2", "stratVar"))
  
  return(df_slope)
}


df_slope <- get_lm_metrics(df_prev_nowork, "NOWORK_COVID")
# df_slope <- get_lm_metrics(df_prev_lostwork, "LOST_WORK")

# # -------------------------------------------------------------------------
# # plots
# 
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



df_corr_region <- calcCovidCorr(df_prev_region, "REGION")
df_corr_lostwork <- calcCovidCorr(df_prev_lostwork, "LOST_WORK")
df_corr_nowork <- calcCovidCorr(df_prev_nowork, "NOWORK_COVID")
df_corr_age3 <- calcCovidCorr(df_prev_age3, "AGE_3")
df_corr_gender <- calcCovidCorr(df_prev_gender, "GENDER")
df_corr_race2 <- calcCovidCorr(df_prev_race2, "RACE_2")
df_corr_edu2 <- calcCovidCorr(df_prev_edu2, "EDU_2")
# df_corr_income4 <- calcCovidCorr(df_prev_income4, "INCOME_4")
df_corr <-df_corr_gender %>% 
  bind_rows(df_corr_age3) %>%
  bind_rows(df_corr_race2) %>%
  bind_rows(df_corr_edu2) %>%
  bind_rows(df_corr_lostwork) %>%
  # bind_rows(df_corr_income4) %>%
  bind_rows(df_corr_region)

write.csv(df_corr,
          file.path(dataPath, 
                    paste0(csvPrefix, "correlationMatrics.csv") ),
          row.names = F)


df_corr <- calcCovidCorr(df_prev_nowork, "NOWORK_COVID")
write.csv(df_corr,
          file.path(dataPath, 
                    paste0(csvPrefix, "correlationMatrics.csv") ),
          row.names = F)

# df_prev_region %>%filter(REGION == "United States") %>%
# ggplot(aes(x=cases_per_mean,y=prop, group = PHASE2, col = PHASE2)) + 
#   geom_point()+
#   geom_smooth(method=lm) +
#   facet_wrap(vars(Outcome), nrow = 1)
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




