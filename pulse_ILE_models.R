rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts_ILE"
setwd(path_wd)

library(tidyverse)
library(purrr)
library(margins)
library(tictoc)
library(survey)
library(RColorBrewer)
source("../Scripts/pulse_process_functions.R")


dataPath <- "../Data/"

importDataPath <- file.path(dataPath, "National_PulseData_ILE")
saveDataPath <- file.path(importDataPath, "NONIMPUTED_RESULTS_20210526_withChildren")

table1fileName <- "Table1_tmp.csv"
subgroupOutFileName <- "Subgroup_aOR_notImputed.csv"
subgroupOutFileName_all <- "Subgroup_aOR_notImputed_all.csv"

figName_OR_subgroup_depression3 <- "aAME_Depression3_StimulusPaymentRecv.png"
figName_OR_subgroup_anxiety3 <- "aAME_Anxiety3_StimulusPaymentRecv.png"
figName_OR_subgroup_depression <- "aAME_Depression_StimulusPaymentRecv.png"
figName_OR_subgroup_anxiety <- "aAME_Anxiety_StimulusPaymentRecv.png"
figName_OR_subgroup_both <- "aAME_AnxietyDepression_StimulusPaymentRecv.png"
figName_OR_subgroup_both3 <- "aAME_AnxietyDepression3_StimulusPaymentRecv.png"
# figName_OR_subgroup_food <- "aAME_FoodInsufficiency_StimulusPaymentRecv.png"
# figName_OR_subgroup_expense <- "aAME_DifficultyExpenses_StimulusPaymentRecv.png"

# for reference
var_include <- c("SCRAM", "WEEK", "EST_ST", "PWEIGHT", "HWEIGHT", "Phase",
                 "midDate", "startDate", "endDate",
                 "ANXIETY_3","ANXIETY_4","ANXIETY_5","ANXIETY_6",
                 "DEPRESSION_3","DEPRESSION_4","DEPRESSION_5","DEPRESSION_6",
                 "MONEYINSUF", "FOODINSUF", "RENT_BHND", 
                 "PHQ4_missing", "MONEYINSUF_missing", "FOODINSUF_missing", "RENT_BHND_missing", 
                 "EIP_RECV",
                 "GENDER","AGE", "AGE_4", "RACE_5", "withCHILDREN", "MS",
                 "EDU_2", "INCOME_4", "LOST_WORK", "NOWORK_COVID")


df_all <- read.csv(file.path(importDataPath,
                    paste0("National_Pulse_Week22_StimulusPayment.csv") ))
                    # paste0("National_Pulse_Week22_StimulusPayment_imputed.csv") ))

N_all <- nrow(df_all)


# -------------------------------------------------------------------------
# variable labels/levels
# -------------------------------------------------------------------------

genderLevels <- c("Female",
                  "Male")

race5Levels <- rev(c("Non-hispanic White",
                 "Non-hispanic Black",
                 "Hispanic",
                 "Non-hispanic Asian",
                 "Other or multiple"))

incomeLevels <- rev(c("0 - 34,999", 
                  "35,000 - 74,999", 
                  "75,000 - 149,999"))

edu2Levels <- c("< College",
                ">= College")


age4Levels <- rev(c("18 - 24",
                "25 - 44",
                "45 - 64"))

lostworkLevels <- c("Household lost employment income", 
                    "Household did not lose employment income")

noworkLevels <- c("No work in past 7 days due to pandemic",
                  "No work in past 7 days unrelated to pandemic",
                  "Had work in past 7 days")

noworkLevels_re <- c("No work due to pandemic",
                     "No work unrelated to pandemic",
                     "Had work in past 7 days")

childLevels <- c("Children in household",
                  "No children in household")

levels_for_stratvar <- c("NoEIP","EIP")
labels_for_stratvar <- c("Did not receive stimulus payment\nin past 7 days",
                         "Received stimulus payment\nin past 7 days")


# -------------------------------------------------------------------------
# missing data

sum(df_all$INCOME_4 == "Missing") / nrow(df_all)
sum(df_all$LOST_WORK == "Missing") / nrow(df_all)
sum(df_all$NOWORK_COVID == "Missing") / nrow(df_all)
sum(df_all$EIP_RECV == "Missing") / nrow(df_all)
sum(df_all$PHQ4_missing) / nrow(df_all)


# -------------------------------------------------------------------------


# filter missing
df_nomissing <- df_all %>%
  filter(INCOME_4 != "Missing") %>%
  filter(LOST_WORK != "Missing") %>%
  filter(NOWORK_COVID != "Missing") %>%
  filter(EIP_RECV != "Missing")  %>%
  filter(!PHQ4_missing) 

N_nomissing <- nrow(df_nomissing)
N_all - N_nomissing


# filter by eligible income
df_eligible <- df_nomissing %>%
  filter((INCOME_4%in%c("0 - 34,999", "35,000 - 74,999") |
            INCOME_4%in%c("0 - 34,999", "35,000 - 74,999", "75,000 - 149,999") & MS == 1 ))

N_income_eligible <- nrow(df_eligible)
N_nomissing - N_income_eligible 


# filter by working age
df_eligible_workingage <- df_eligible %>%
  filter(AGE < 65)

N_income_eligible_workingage <- nrow(df_eligible_workingage)
N_income_eligible - N_income_eligible_workingage


# full anlaytic sample
# relevel factors 
df <- df_eligible_workingage %>% 
  mutate(RACE_5 = factor(RACE_5, 
                         levels = race5Levels  ) ) %>%
  mutate(INCOME_4 = factor(INCOME_4, 
                         levels = incomeLevels  ) ) %>%
  mutate(LOST_WORK = factor(LOST_WORK, 
                         levels = lostworkLevels) ) %>%
  mutate(AGE_4 = factor(AGE_4, 
                            levels = age4Levels ) ) %>%
  mutate(NOWORK_COVID = factor(NOWORK_COVID, 
                         levels = noworkLevels,
                         labels = noworkLevels_re ) ) %>%
  mutate(EIP_RECV = factor(EIP_RECV, 
                           levels = levels_for_stratvar,
                           labels = labels_for_stratvar) )  %>%
  mutate(EST_ST = factor(EST_ST))





# -------------------------------------------------------------------------
# Table 1
# -------------------------------------------------------------------------


# chi-square test

round( chisq.test(df$EIP_RECV, df$GENDER)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$withCHILDREN)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$RACE_5)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$INCOME_4)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$EDU_2)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$AGE_4)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$MS)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$LOST_WORK)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$NOWORK_COVID)$p.value , digits=3)


calc_var_n_prop <- function(df, groupVar){
  # get proportions of each group
  df_prop <- data.frame( round( ( prop.table(table(df$EIP_RECV, df[[groupVar]]),2) ) * 100 , digits = 1)) %>%
    rename(EIP = Var1 ,
           group = Var2,
           prop = Freq)
  
  # get counts of each group
  df_n <- df %>%
    count(!!sym(groupVar) )
  names(df_n) <- c("group", "n")
  
  
  df_long <- data.frame( table(df$EIP_RECV, df[[groupVar]])  ) %>%
    rename(EIP = Var1 ,
           group = Var2,
           n = Freq) %>%
    left_join(df_prop, by = c("EIP", "group")) %>%
    mutate(n_prop_str = paste0(n, " (", prop, "%)"))
  
  
  df_table1_formatted <- df_long %>%
    select(c("EIP", "group", "n_prop_str")) %>%
    filter(EIP == "Received stimulus payment\nin past 7 days") %>%
    # group_by(group) %>%
    pivot_wider(names_from = EIP, values_from = n_prop_str) %>%
    right_join(df_n, by = "group") 
  
  
  return(df_table1_formatted)
}

df_table1 <- c("GENDER", "AGE_4", "RACE_5", "EDU_2", "INCOME_4", "LOST_WORK", "NOWORK_COVID") %>%
  map_df( ~ calc_var_n_prop(df, .x) )
df_table1 <- c("GENDER", "AGE_4", "RACE_5", "EDU_2", "INCOME_4", "LOST_WORK", "NOWORK_COVID", "withCHILDREN") %>%
  map_df( ~ calc_var_n_prop(df, .x) )


write.csv(df_table1, file.path(saveDataPath, table1fileName), row.names = FALSE )


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# regression models
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


run_mdl <- function(svydsn, outcomeStr, covars){
  
  formula_reg <- paste(outcomeStr, "~", paste(covars, collapse="+"))
  
  
  mdl <- svyglm( as.formula(formula_reg) , design=svydsn,
                   family=quasibinomial() ) 

# -------------------------------------------------------------------------

  
  marg_mdl <- margins(mdl, design = svydsn)

  df_mdl <- gen_AME_df(marg_mdl)

  df_mdl_out <- df_mdl %>%
    mutate(Outcome = outcomeStr)

# -------------------------------------------------------------------------

  
  # df_mdl <- gen_mdl_df(mdl)
  # 
  # df_mdl_out <- df_mdl %>%
  #   mutate(Outcome = outcomeStr) %>%
  #   filter(Variable != "(Intercept)")

# -------------------------------------------------------------------------

  return(df_mdl_out)
}





run_all_mdls <- function(svydsn, covars){
  
  df_mdl_out <- c(
                  "ANXIETY_3", "DEPRESSION_3", 
                  # "ANXIETY_4", "DEPRESSION_4", 
                  "ANXIETY_5", "DEPRESSION_5" 
                  # "ANXIETY_6", "DEPRESSION_6", 
                  # "FOODINSUF", "MONEYINSUF"
                  ) %>%
    map_df(~ run_mdl(svydsn, .x, covars) )
  
  
  return(df_mdl_out)
           
}


# run_anx_dep_mdl <- function(svydsn, outcomeStr, covars){
#   
#   formula_reg <- paste("y ~", paste(covars, collapse="+"))
#   
#   
#   mdl_a <- svyglm( as.formula(formula_reg) , design=svydsn,
#                   family=quasibinomial() ) 
#   df_mdl_a <- gen_mdl_df(mdl_a)
#   
#   mdl_d <- svyglm(DEPRESSION_5 ~ EIP_RECV + AGE_4 + EDU_2 + RACE_5 + INCOME_4 + EST_ST, design=svydsn,
#                   family=quasibinomial() ) 
#   df_mdl_d <- gen_mdl_df(mdl_d)
#   
#   mdl_f <- svyglm(FOODINSUF ~ EIP_RECV + AGE_4 + EDU_2 + RACE_5 + INCOME_4 + EST_ST, design=svydsn,
#                   family=quasibinomial() ) 
#   df_mdl_f <- gen_mdl_df(mdl_f)
#   
#   mdl_e <- svyglm(MONEYINSUF ~ EIP_RECV + AGE_4 + EDU_2 + RACE_5 + INCOME_4 + EST_ST, design=svydsn,
#                   family=quasibinomial() ) 
#   df_mdl_e <- gen_mdl_df(mdl_e)
#   
#   
#   df_mdl_a$Outcome = "ANXIETY_5"
#   df_mdl_d$Outcome = "DEPRESSION_5"
#   df_mdl_f$Outcome = "FOODINSUF"
#   df_mdl_e$Outcome = "MONEYINSUF"
#   
#   df_mdl <- df_mdl_a %>%
#     bind_rows(df_mdl_d) %>%
#     bind_rows(df_mdl_f) %>%
#     bind_rows(df_mdl_e) %>%
#     filter(Variable != "(Intercept)") 
#   
#   return(df_mdl)
# }


# -------------------------------------------------------------------------
#functions for subgroup models
# -------------------------------------------------------------------------



gen_mdl_gender <- function(df, genderLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(GENDER == genderLevel))
  
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV","AGE_4","RACE_5", "EDU_2", "INCOME_4","withCHILDREN", "EST_ST")
  }

  
  df_mdl <- run_all_mdls(svydsn, covars) %>%
    mutate(stratVar = genderLevel,
           varCategory = "Gender\nIdentity")
  
  return(df_mdl)
}

gen_mdl_age <- function(df, ageLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(AGE_4 == ageLevel))
  
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "RACE_5", "EDU_2", "INCOME_4","withCHILDREN", "EST_ST")
  }

  
  df_mdl <- run_all_mdls(svydsn, covars) %>%
    mutate(stratVar = ageLevel,
           varCategory = "Age")
  
  return(df_mdl)
}


gen_mdl_race <- function(df, raceLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(RACE_5 == raceLevel))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4", "EDU_2", "INCOME_4","withCHILDREN", "EST_ST")
  }

  
  df_mdl <- run_all_mdls(svydsn, covars) %>%
    mutate(stratVar = raceLevel,
           varCategory = "Race/Ethnicity")
  
  return(df_mdl)
}
gen_mdl_edu<- function(df, eduLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(EDU_2 == eduLevel))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "INCOME_4","withCHILDREN", "EST_ST")
  }

  
  df_mdl <- run_all_mdls(svydsn, covars) %>%
    mutate(stratVar = eduLevel,
           varCategory = "Education")
  
  return(df_mdl)
}
gen_mdl_income<- function(df, incomeLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(INCOME_4 == incomeLevel))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2","withCHILDREN", "EST_ST")
  }

  
  df_mdl <- run_all_mdls(svydsn, covars) %>%
    mutate(stratVar = incomeLevel,
           varCategory = "Household\nIncome")
  
  return(df_mdl)
}

gen_mdl_lostwork<- function(df, lostworkLevels, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(LOST_WORK == lostworkLevels))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4","withCHILDREN", "EST_ST")
  }

  df_mdl <- run_all_mdls(svydsn, covars) %>%
    mutate(stratVar = lostworkLevels,
           varCategory = "Lost Income")
  
  return(df_mdl)
}

gen_mdl_nowork<- function(df, noworkLevels, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(NOWORK_COVID == noworkLevels))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4","withCHILDREN", "EST_ST")
  }

  df_mdl <- run_all_mdls(svydsn, covars) %>%
    mutate(stratVar = noworkLevels,
           varCategory = "Work Status")
  
  return(df_mdl)
}

gen_mdl_child<- function(df, childLevels, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(withCHILDREN == childLevels))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4", "EST_ST")
  }

  
  df_mdl <- run_all_mdls(svydsn, covars) %>%
    mutate(stratVar = childLevels,
           varCategory = "Children")
  
  return(df_mdl)
}


# -------------------------------------------------------------------------
# run impact models
# -------------------------------------------------------------------------


# survey design
svydsn <- svydesign(ids = ~SCRAM,
                    weights = ~PWEIGHT,
                    nest = T,
                    data = df)



covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4", "withCHILDREN", "EST_ST")
covars_part <- c("EIP_RECV", "EDU_2", "EST_ST")
model_names <-  c("Fully Adjusted", "Partially Adjusted", "Crude")

tic()
df_mdl_crude <- run_all_mdls(svydsn, "EIP_RECV") %>%
  # mutate(stratVar = "Crude",
  mutate(stratVar = "Full Sample",
         varCategory = "")
toc()
# 
# tic()
# df_mdl_adj1 <- run_all_mdls(svydsn, covars_part) %>%
#   mutate(stratVar = "Partially Adjusted",
#          varCategory = "Full Sample")
# toc()
# 
# tic()
# df_mdl_adj2 <- run_all_mdls(svydsn, covars) %>%
#   mutate(stratVar = "Fully Adjusted",
#          varCategory = "Full Sample")
# toc()


# -------------------------------------------------------------------------

# run subgroup analysis
# -------------------------------------------------------------------------
modelType <- "crude"

df_mdl_gender <- genderLevels %>%
  map_df(~ gen_mdl_gender(df, .x, modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, levels = genderLevels))

df_mdl_age <- age4Levels %>%
  map_df(~ gen_mdl_age(df, .x, modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, levels = age4Levels))

df_mdl_race <- race5Levels %>%
  map_df(~ gen_mdl_race(df, .x, modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, levels = race5Levels))

df_mdl_income <- incomeLevels %>%
  map_df(~ gen_mdl_income(df, .x, modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, levels = incomeLevels))

df_mdl_edu <- edu2Levels %>%
  map_df(~ gen_mdl_edu(df, .x, modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, levels = edu2Levels))


df_mdl_lostwork <- lostworkLevels %>%
  map_df(~ gen_mdl_lostwork(df, .x, modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, levels = lostworkLevels))


df_mdl_nowork <- noworkLevels_re %>%
  map_df(~ gen_mdl_nowork(df, .x, modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, levels = noworkLevels_re))

df_mdl_child <- childLevels %>%
  map_df(~ gen_mdl_child(df, .x, modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, levels = childLevels))








# -------------------------------------------------------------------------




  # run_anx_dep_mdl(svydsn) %>%
  # mutate(stratVar = "Full Analytic Sample",
         # varCategory = "")

# # survey design
# svydsn <- svydesign(ids = ~SCRAM,
#                     weights = ~PWEIGHT,
#                     nest = T,
#                     data = df )


# -------------------------------------------------------------------------
# bind results from all models
# -------------------------------------------------------------------------




df_mdl_all_all <- df_mdl_crude %>%
  # bind_rows(df_mdl_adj1) %>%
  # bind_rows(df_mdl_adj2) %>%
  # mutate(varCategory = factor(varCategory, levels = c("Full Sample")) ) %>% 
  # mutate(stratVar = factor(stratVar, c(model_names) ) ) 

  bind_rows(df_mdl_gender) %>%
  bind_rows(df_mdl_age) %>%
  bind_rows(df_mdl_race) %>%
  bind_rows(df_mdl_edu) %>%
  bind_rows(df_mdl_income) %>%
  bind_rows(df_mdl_child) %>%
  # bind_rows(df_mdl_lostwork) %>%
  bind_rows(df_mdl_nowork) %>%
  mutate(varCategory = factor(varCategory, levels = c("", "Gender\nIdentity", "Age", "Race/Ethnicity", "Education","Household\nIncome", "Children", "Work Status")) ) %>%
  mutate(stratVar = factor(stratVar, c("Full Sample", genderLevels, age4Levels, edu2Levels, race5Levels, incomeLevels, childLevels, noworkLevels_re) ) )

df_mdl_all <- df_mdl_all_all %>%
  filter(startsWith(Variable, "EIP_RECV"))

write.csv(df_mdl_all, file.path(saveDataPath, subgroupOutFileName), row.names = FALSE )
write.csv(df_mdl_all_all, file.path(saveDataPath, subgroupOutFileName_all), row.names = FALSE )



# -------------------------------------------------------------------------
# crude model -- model code (for troubleshooting)
# -------------------------------------------------------------------------

# mdl_a <- svyglm(ANXIETY_5 ~ EIP_RECV , design=svydsn, 
#                 family=quasibinomial() ) 
# marg_mdl_a <- margins(mdl_a, design = svydsn) 
# df_AME_a<-gen_AME_df(marg_mdl_a)
# df_mdl_a <- gen_mdl_df(mdl_a)
# 
# mdl_d <- svyglm(DEPRESSION_5 ~ EIP_RECV , design=svydsn, #  + AGE_4 + EDU_2 + RACE_5 + INCOME_4
#                 family=quasibinomial() ) 
# marg_mdl_d <- margins(mdl_d, design = svydsn) 
# df_AME_d<-gen_AME_df(marg_mdl_d)
# df_mdl_d <- gen_mdl_df(mdl_d)
# 
# df_mdl_a$Outcome = "ANXIETY_5"
# df_mdl_d$Outcome = "DEPRESSION_5"
# 
# df_mdl_all_crude <- df_mdl_a %>%
#   bind_rows(df_mdl_d) %>%
#   filter(startsWith(Variable, "EIP_RECV"))



# -------------------------------------------------------------------------
# partially adjusted model code (for troubleshooting)
# -------------------------------------------------------------------------

# mdl_a <- svyglm(ANXIETY_5 ~ EIP_RECV + EDU_2 + EST_ST , design=svydsn, 
#                 family=quasibinomial() ) 
# df_mdl_a <- gen_mdl_df(mdl_a)
# 
# mdl_d <- svyglm(DEPRESSION_5 ~ EIP_RECV + EDU_2 + EST_ST, design=svydsn, #  + AGE_4 + EDU_2 + RACE_5 + INCOME_4
#                 family=quasibinomial() ) 
# 
# tic()
# marg_mdl_d <- margins(mdl_d, design = svydsn) 
# toc()
# 
# df_mdl_d <- gen_mdl_df(mdl_d)
# 
# df_mdl_a$Outcome = "ANXIETY_5"
# df_mdl_d$Outcome = "DEPRESSION_5"
# 
# df_mdl_all_adj1 <- df_mdl_a %>%
#   bind_rows(df_mdl_d) %>%
#   filter(startsWith(Variable, "EIP_RECV"))



# -------------------------------------------------------------------------
# fully adjusted model code (for troubleshooting)
# -------------------------------------------------------------------------


# mdl_a <- svyglm(ANXIETY_5 ~ EIP_RECV + AGE_4 + EDU_2 + RACE_5 + INCOME_4 + withCHILDREN + EST_ST , design=svydsn, 
#                 family=quasibinomial() ) 
# 
# df_mdl_a <- gen_mdl_df(mdl_a)
# 
# mdl_d <- svyglm(DEPRESSION_5 ~ EIP_RECV + AGE_4 + EDU_2 + RACE_5 + INCOME_4 + withCHILDREN + EST_ST, design=svydsn, #  + AGE_4 + EDU_2 + RACE_5 + INCOME_4
#                 family=quasibinomial() ) 
# tic()
# marg_mdl_d <- margins(mdl_d, design = svydsn) 
# toc()
# 
# df_AME_d<-gen_AME_df(marg_mdl_d)
# df_mdl_d <- gen_mdl_df(mdl_d)
# 
# df_mdl_a$Outcome = "ANXIETY_5"
# df_mdl_d$Outcome = "DEPRESSION_5"
# 
# df_mdl_all_adj2 <- df_mdl_a %>%
#   bind_rows(df_mdl_d) %>%
#   filter(startsWith(Variable, "EIP_RECV"))



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# function for displaying results
displayModelResults <- function(mdl)
{
  
  if(is.data.frame(mdl) ){
    df <- mdl
  }
  else{
    df <- gen_mdl_df(mdl)
  }  
  
  
  
  p1 <- df %>%    
    # ggplot(aes(x = stratVar, y = OR, ymin = CI_l, ymax = CI_u)) +
    # geom_hline(aes(yintercept = 1), linetype = 'dashed') +
    ggplot(aes(x = stratVar, y = AME, ymin = CI_l, ymax = CI_u)) +
    geom_hline(aes(yintercept = 0), linetype = 'dashed') +

    geom_pointrange( aes(col = varCategory), size = 1) +
    theme_minimal() +
    coord_flip() +
    facet_grid(row = vars(varCategory), scales="free_y", space = "free_y") +
    theme(
          legend.position = "none",
          text = element_text(size = 16)
          # axis.text.x = element_text(angle = 45, hjust=1)
          # axis.text.x = element_text(angle = 45, hjust=1)
          # panel.spacing = unit(2, "lines")
          ) +
    scale_color_brewer(palette = "Dark2") +
    xlab("")
  # scale_y_continuous(trans='log2') +
  # coord_flip()
  
  
  return(p1)
}


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

p_a <- displayModelResults(df_mdl_all %>% filter(Outcome == "ANXIETY_5" &  startsWith(Variable, "EIP_RECV") )) +
  # scale_y_continuous(trans='log2', limits = c(.30, 1.6)) +
  ylab("Average Marginal Effect (95% CI)\nEffect of Stimulus Payment\non Anxiety Symptoms (GAD-2>=5)")
p_a

ggsave(file.path(saveDataPath, figName_OR_subgroup_anxiety) ,
       plot = p_a,
       # width = 14,
       # height = 8,
       width = 6,
       height = 10,
       device = "png")

# -------------------------------------------------------------------------

p_d <- displayModelResults(df_mdl_all %>% filter(Outcome == "DEPRESSION_5" &  startsWith(Variable, "EIP_RECV") )) +
  # scale_y_continuous(trans='log2', limits = c(.30, 1.63)) +
  ylab("Average Marginal Effect (95% CI)\nEffect of Stimulus Payment\non Depression Symptoms (PHQ-2>=5)")
p_d


ggsave(file.path(saveDataPath, figName_OR_subgroup_depression) ,
       plot = p_d,
       width = 6,
       height = 10,
       device = "png")

# both anx and depression
p_both <- df_mdl_all %>%
  filter(Outcome %in% c("ANXIETY_5", "DEPRESSION_5") &  startsWith(Variable, "EIP_RECV") ) %>%
  mutate(Outcome = factor(Outcome, 
                          levels = c("ANXIETY_5", "DEPRESSION_5"),
                          labels = c("Anxiety Symptoms\n(GAD-2 >= 5)",
                                     "Depression Symptoms\n(PHQ-2 >= 5)") ) ) %>%
  displayModelResults() +
  facet_grid(row = vars(varCategory), col = vars(Outcome), scales="free_y", space = "free_y") +
  theme(panel.spacing.x = unit(2, "lines")) +
  # scale_y_continuous(trans='log2', limits = c(.30, 1.6)) +
  ylab("Crude Impact Estimate of Stimulus Payment\nAverage Marginal Effect (95% CI)")
p_both

ggsave(file.path(saveDataPath, figName_OR_subgroup_both) ,
       plot = p_both,
       width = 12,
       height = 10,
       device = "png")


# -------------------------------------------------------------------------
# score threshold = 3
# -------------------------------------------------------------------------



p_a <- displayModelResults(df_mdl_all %>% filter(Outcome == "ANXIETY_3" &  startsWith(Variable, "EIP_RECV") )) +
  # scale_y_continuous(trans='log2', limits = c(.30, 1.8)) +
  ylab("OR (95% CI)\nEffect of Stimulus Payment\non Anxiety Symptoms (GAD-2>=3)")
  # ggtitle("Relationship Between Stimulus Payment Receipt and Anxiety Symptoms (GAD-2 >= 5)")
p_a

ggsave(file.path(saveDataPath, figName_OR_subgroup_anxiety3) ,
       plot = p_a,
       # width = 14,
       # height = 8,
       width = 6,
       height = 10,
       device = "png")

# -------------------------------------------------------------------------

p_d <- displayModelResults(df_mdl_all %>% filter(Outcome == "DEPRESSION_3" &  startsWith(Variable, "EIP_RECV") )) +
  # scale_y_continuous(trans='log2', limits = c(.30, 1.63)) +
  ylab("OR (95% CI)\nEffect of Stimulus Payment\non Depression Symptoms (PHQ-2>=3)")
  # ggtitle("Relationship Between Stimulus Payment Receipt and Depression Symptoms (PHQ-2 >= 5)")
p_d


ggsave(file.path(saveDataPath, figName_OR_subgroup_depression3) ,
       plot = p_d,
       width = 6,
       height = 10,
       device = "png")



# both anx and depression
p_both3 <- df_mdl_all %>%
  filter(Outcome %in% c("ANXIETY_3", "DEPRESSION_3") &  startsWith(Variable, "EIP_RECV") ) %>%
  mutate(Outcome = factor(Outcome, 
                          levels = c("ANXIETY_3", "DEPRESSION_3"),
                          labels = c("Anxiety Symptoms\n(GAD-2 >= 3)",
                                     "Depression Symptoms\n(PHQ-2 >= 3)") ) ) %>%
  displayModelResults() +
  facet_grid(row = vars(varCategory), col = vars(Outcome), scales="free_y", space = "free_y") +
  theme(panel.spacing.x = unit(2, "lines")) +
  # scale_y_continuous(trans='log2', limits = c(.30, 1.6)) +
  ylab("Crude Impact Estimate of Stimulus Payment\nAverage Marginal Effect (95% CI)")
p_both3

ggsave(file.path(saveDataPath, figName_OR_subgroup_both3) ,
       plot = p_both3,
       width = 12,
       height = 10,
       device = "png")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------





# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------


# p_f <- displayModelResults(df_mdl_all %>% filter(Outcome == "FOODINSUF" &  startsWith(Variable, "EIP_RECV") )) +
#   scale_y_continuous(trans='log2', limits = c(.20, 1.9)) +
#   ylab("OR (95% CI)\nEffect of Stimulus Payment\non Food Insufficiency") 
#   # ggtitle("Relationship Between Stimulus Payment Receipt and Food Insuffiency")
# p_f
# 
# ggsave(file.path(saveDataPath, figName_OR_subgroup_food) , 
#        plot = p_f,
#        width = 6, 
#        height = 10,
#        device = "png")
# 
# 
# # -------------------------------------------------------------------------
# 
# p_e <- displayModelResults(df_mdl_all %>% filter(Outcome == "MONEYINSUF" &  startsWith(Variable, "EIP_RECV") )) +
#   scale_y_continuous(trans='log2', limits = c(.25, 1.5)) +
#   ylab("OR (95% CI)\nEffect of Stimulus Payment\non Difficulty with Expenses") 
#   # ggtitle("Relationship Between Stimulus Payment Receipt and Difficulty with Expenses")
# p_e
# 
# ggsave(file.path(saveDataPath, figName_OR_subgroup_expense) , 
#        plot = p_e,
#        width = 6, 
#        height = 10,
#        device = "png")
# 
# 
# # library(grid)
# # gt = ggplot_gtable(ggplot_build(g))
# # gt$widths[4] = 4*gt$widths[4]
# # grid.draw(gt)
# 
# 
# # -------------------------------------------------------------------------
# # plot trade space (exploratory)
# # -------------------------------------------------------------------------
# 
# df_mdl_all %>%
#   filter( Outcome %in% c("ANXIETY_5", "MONEYINSUF") ) %>%
#   select( c("OR", "Outcome", "stratVar", "varCategory") ) %>%
#   pivot_wider(names_from = Outcome,  values_from = OR, names_sep = "_") %>%
#   ggplot(aes(x = MONEYINSUF, y = ANXIETY_5, col = varCategory)) +
#   geom_point() +
#   geom_text(aes(label = stratVar), nudge_x = 0.00, nudge_y = -0.01, size = 3) +
#   theme_bw() +
#   scale_color_brewer(palette = "Dark2") 
# 
# df_mdl_all %>%
#   filter( Outcome %in% c("DEPRESSION_5", "MONEYINSUF") ) %>%
#   select( c("OR", "Outcome", "stratVar", "varCategory") ) %>%
#   pivot_wider(names_from = Outcome,  values_from = OR, names_sep = "_") %>%
#   ggplot(aes(x = MONEYINSUF, y = DEPRESSION_5, col = varCategory)) +
#   geom_point() +
#   geom_text(aes(label = stratVar), nudge_x = 0.00, nudge_y = -0.015, size = 3) +
#   theme_bw() +
#   scale_color_brewer(palette = "Dark2") 
# 
# 
# df_mdl_all %>%
#   filter( Outcome %in% c("ANXIETY_5", "FOODINSUF") ) %>%
#   select( c("OR", "Outcome", "stratVar", "varCategory") ) %>%
#   pivot_wider(names_from = Outcome,  values_from = OR, names_sep = "_") %>%
#   ggplot(aes(x = FOODINSUF, y = ANXIETY_5, col = varCategory)) +
#   geom_point() +
#   geom_text(aes(label = stratVar), nudge_x = 0.00, nudge_y = -0.01, size = 3) +
#   theme_bw() +
#   scale_color_brewer(palette = "Dark2") 
# 
# df_mdl_all %>%
#   filter( Outcome %in% c("DEPRESSION_5", "FOODINSUF") ) %>%
#   select( c("OR", "Outcome", "stratVar", "varCategory") ) %>%
#   pivot_wider(names_from = Outcome,  values_from = OR, names_sep = "_") %>%
#   ggplot(aes(x = FOODINSUF, y = DEPRESSION_5, col = varCategory)) +
#   geom_point() +
#   geom_text(aes(label = stratVar), nudge_x = 0.00, nudge_y = -0.015, size = 3) +
#   theme_bw() +
#   scale_color_brewer(palette = "Dark2") 
# 
# 
# df_mdl_all %>%
#   filter( Outcome %in% c("DEPRESSION_5", "ANXIETY_5") ) %>%
#   select( c("OR", "Outcome", "stratVar", "varCategory") ) %>%
#   pivot_wider(names_from = Outcome,  values_from = OR, names_sep = "_") %>%
#   ggplot(aes(x = ANXIETY_5, y = DEPRESSION_5, col = varCategory)) +
#   geom_point() +
#   geom_text(aes(label = stratVar), nudge_x = 0.00, nudge_y = -0.015, size = 3) +
#   theme_bw() +
#   scale_color_brewer(palette = "Dark2") 




