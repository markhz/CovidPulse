# specific variable to stratify by
# 



rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts"
setwd(path_wd)

library(tictoc)
library(zoo)
library(tidyverse)
library(purrr)
library(survey)
require(egg)
source("pulse_process_functions.R")


# -------------------------------------------------------------------------
# set paths
# -------------------------------------------------------------------------


# data path
dataPath <- "../Data/National_PulseData_Paper1"
refDataPath <- file.path(dataPath, "..")

# paths to reference tables
filePath_var_OI <- file.path(dataPath, "var_OI_paper1.csv")
filePath_weektime <- file.path(refDataPath, "pulse_weeknum_to_date.csv")

importCsvFileName <- "National_Pulse_imported_imputed.csv"

saveFilePrefix <- "National_Pulse_pweightPrevImputed_workingage_by"
# saveFilePrefix <- "National_Pulse_pweightPrevImputed_workingage1_by"
# saveFilePrefix <- "National_Pulse_pweightPrevImputed_workingage2_by"
# saveFilePrefix <- "National_Pulse_pweightPrevImputed_REGION1_by"
# saveFilePrefix <- "National_Pulse_pweightPrevImputed_by"


df_states <- read.csv(file.path(refDataPath, "pulse_states.csv")) %>%
  mutate(REGION = as.factor(REGION))
# -------------------------------------------------------------------------
# set variables
# -------------------------------------------------------------------------


# # identify outcome variables
# outcomeVar <- c("PHQ4_SEVERE",
#                 "ANXIETY",
#                 "DEPRESSION",
#                 "ANXIETY_2",
#                 "DEPRESSION_2")
# # rename outcome factors
# outcomeRename <- c("Severe PHQ-4 Score (>= 9)",
#                    "High GAD-2 Score (>= 5)",
#                    "High PHQ-2 Score (>= 5)",
#                    "Two Symptoms of Anxiety",
#                    "Two Symptoms of Depression")

# identify outcome variables
outcomeVar <- c("ANXIETY",
                "DEPRESSION")
# rename outcome factors
outcomeRename <- c("High GAD-2 Score (>= 5)",
                   "High PHQ-2 Score (>= 5)")

# outcomeVar <- c("PHQ4_missing")
#
# outcomeRename <- c("Missing PHQ-4 Score")




# -------------------------------------------------------------------------


# import table indicating variables to import
df_var <- import_clean_var_OI( filePath_var_OI )

# import table with survey timing info
df_weektime <- import_clean_weektime( filePath_weektime )


# -------------------------------------------------------------------------
# load pulse data and construct new variables
# -------------------------------------------------------------------------

# import selected pulse data
df_import <- read.csv(file.path(dataPath, importCsvFileName)) %>%
  # add updated state file to imputed dataset
  select(-c("STATE", "StateAbbr", "REGION") ) %>%
  left_join(df_states, by = c("EST_ST" = "StateValue"))

# constructs variables in imported Pulse data for analysis
df <- construct_new_pulse_var(df_import)

sum(df$NOWORK_COVID_missing) / nrow(df)

df <- df %>%
  filter(!PHQ4_missing  & LOST_WORK != "Missing") %>%
  # filter(FEMA_REGION == "REGION 1")
  filter(AGE < 65 & !NOWORK_COVID_missing)
  # filter(AGE < 40)
  # filter(AGE >= 40 & AGE < 65)


rm(df_import)
# -------------------------------------------------------------------------

df$PLACEHOLDER = 1

# VARIABLES TO STRATIFY ON... and set factor levels

# strat_variables <- "STATE"
# levels_for_stratvar <- c("Connecticut", "Maine", "Massachusetts", 
                         # "New Hampshire", "Rhode Island", "Vermont")
strat_variables <- c("REGION", "GENDER", "EDU_2", "AGE_3","AGE_4", "RACE_2", "RACE_5", "NOWORK_COVID", "LOST_WORK")#, "INCOME_4"
levels_for_stratvar<-list(
                          c("Full Sample","Northeast","South","Midwest","West"),
                          c( "Male", "Female"),
                          c( "< College", ">= College"),
                          c("18 - 39", "40 - 64",">= 65"),
                          c("18 - 24", "25 - 44", "45 - 64",">= 65"),
                          c( "White", "Non-white"),
                          c( "Non-hispanic White", "Non-hispanic Black",
                            "Hispanic", "Non-hispanic Asian", "Other or multiple"),
                          c("No work in past 7 days due to pandemic", "No work in past 7 days unrelated to pandemic", "Had work in past 7 days"),
                          c( "Household lost employment income", "Household did not lose employment income")
                          # c("0 - 34,999","35,000 - 74,999","75,000 - 149,999", "> 150,000")
                          )


# -------------------------------------------------------------------------
#  summarize missing data and filter data frame
# -------------------------------------------------------------------------


# # filter and create survey design
# df <- df %>%
#   filter(!PHQ4_missing & LOST_WORK != "Missing")

# svydsn <- svydesign(ids = ~SCRAM,
#                     weights = ~df$PWEIGHT,
#                     nest = T,
#                     data = df)

# for(i in 1:length(strat_variables) ){
for(i in (length(strat_variables)-1):length(strat_variables) )  {
# for(i in 5) {
  
  tic()
  
  stratVar <- strat_variables[i]
  levels_stratVar <- levels_for_stratvar[[i]]
  
  print(stratVar)
  
  # # filter missing data (outcomes or variable for stratification)
  # df <- df %>%
  #   filter(!PHQ4_missing & LOST_WORK != "Missing")
  
  
  if(stratVar == "REGION") {
    include_fullsample = TRUE
  } else {
    include_fullsample = FALSE
  }
  
  # -------------------------------------------------------------------------
  # survey-weighted prevalence
  # -------------------------------------------------------------------------
  

  # svydsn <- svydesign(ids = ~SCRAM,
  #                     weights = ~df$PWEIGHT,
  #                     nest = T,
  #                     data = df)
  
  

  # if(RUN_BY_WEEK){
  

  # get survey weighted prevalence -- map to each data collection period (WEEK)
  df_prev_svtwt <- unique(df$WEEK) %>%
    map_df(~ map_svywt_prop_to_week(df, 
                                    .x, 
                                    by = stratVar, 
                                    outcomeVar = outcomeVar, 
                                    BY_WEEK = TRUE, 
                                    INCLUDE_FULL_SAMPLE = include_fullsample) )
  
  df_prev_svtwt <- df_prev_svtwt %>%
    left_join(df_weektime, by = "WEEK")
  
  # df_prev_svtwt <- outcomeVar %>%
  #   map_df(~ get_svywt_prop_strat(svydsn, .x, by = stratVar, BY_WEEK = TRUE, INCLUDE_FULL_SAMPLE = include_fullsample) )
  
  
  
  
  # } else {
  #   df_prev_svtwt <- outcomeVar %>%
  #     map_df(~ get_svywt_prop_strat(svydsn, .x, by = stratVar, BY_WEEK = FALSE, INCLUDE_FULL_SAMPLE = include_fullsample) )
  #   
  #   saveFilePrefix <- "National_Pulse_pweightPrevalence_fullsample_by"
  # }
  

  # rename outcome variable levels  
  df_prev_svtwt <- df_prev_svtwt %>%
    mutate(Outcome = factor(Outcome,
                            levels = outcomeVar,
                            labels = outcomeRename ) ) %>%
    arrange(Outcome, !!sym(stratVar), WEEK)

  
  # reset levels for variables used to stratify
  # levels(df_prev_svtwt[[stratVar]] ) <- levels_stratVar
  
  
  
  # save prevalence table
  write.csv(df_prev_svtwt,
            file.path(dataPath, 
                      paste0(saveFilePrefix, stratVar, ".csv") ),
            row.names = F)
  
  
  toc()
  
}









