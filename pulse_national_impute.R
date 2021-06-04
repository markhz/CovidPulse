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
library(mice)
require(egg)
source("pulse_process_functions.R")


# -------------------------------------------------------------------------
# set paths
# -------------------------------------------------------------------------


# data path
# dataPath <- "../Data/National_PulseData_Paper1"
dataPath <- "../Data/National_PulseData_ILE"
fileNameIn <- "National_Pulse_imported.csv"
fileNameOut <- "National_Pulse_imported_imputed_ILE22.csv"

# -------------------------------------------------------------------------
# load pulse data and construct new variables
# -------------------------------------------------------------------------

# import selected pulse data
df_import <- read.csv(file.path(dataPath, fileNameIn)) 
# -------------------------------------------------------------------------
df_import <- df_import %>%
  filter(WEEK == 22)
# -------------------------------------------------------------------------





var2impute <- c("EST_ST", "PWEIGHT", "TBIRTH_YEAR","EGENDER","RHISPANIC","RRACE","EEDUC","THHLD_NUMPER","THHLD_NUMKID",
                # "EIP", "INCOME", "CURFOODSUF","EXPNS_DIF", 
                "WRKLOSS", 
                "ANXIOUS", "WORRY", "DOWN", "INTEREST") 

categoryVar <- c("EST_ST","EGENDER","RHISPANIC","RRACE", "EEDUC",
                 # "EIP", "INCOME", "CURFOODSUF","EXPNS_DIF",
                 "WRKLOSS",
                 "ANXIOUS", "WORRY", "DOWN", "INTEREST")




# impute before constructing variables...
imputeMissingData <- function(df, WEEK_num, var2impute, categoryVar){
  
  print(WEEK_num)
  
  # -------------------------------------------------------------------------
  # preprocess
  # -------------------------------------------------------------------------
  
  # get week of interest
  df <- df %>%
    filter(WEEK == WEEK_num)

  df_imputed <- df
  

# -------------------------------------------------------------------------

  
  # get variables to impute
  df <- df %>%
    select(var2impute)

  
  # convert missing values (-88, -99) to NA
  df[df < 0] <- NA
  
  # convert categorical variables to factors
  df <- df %>%
    mutate_at(categoryVar, factor)
  
  
  # -------------------------------------------------------------------------
  # impute
  # -------------------------------------------------------------------------
  
  # IMPUTE - design model
  mdl_impute <- mice(df,
                     seed=500)
  # complete imputation
  df_imputed_complete <- complete(mdl_impute)
  

# -------------------------------------------------------------------------

  
  
  df_imputed$WRKLOSS <- df_imputed_complete$WRKLOSS
  df_imputed$ANXIOUS <- df_imputed_complete$ANXIOUS
  df_imputed$WORRY <- df_imputed_complete$WORRY
  df_imputed$DOWN <- df_imputed_complete$DOWN
  df_imputed$INTEREST <- df_imputed_complete$INTEREST
  # df_imputed$EIP <- df_imputed_complete$EIP
  # df_imputed$INCOME <- df_imputed_complete$INCOME
  # df_imputed$CURFOODSUF <- df_imputed_complete$CURFOODSUF
  # df_imputed$EXPNS_DIF <- df_imputed_complete$EXPNS_DIF
  
  return(df_imputed)
}





# df_imputed <- c(1,2) %>%
#   map_df(~ imputeMissingData( df_import[c(1:200,107319:(107319 + 199)),] ,
#                              .x,
#                              var2impute = var2impute,
#                              categoryVar = categoryVar) )


# imputation

df_imputed <- unique(df_import$WEEK) %>%
# df_imputed <- c(26, 27) %>%
  map_df(~ imputeMissingData(df_import, 
                             .x, 
                             var2impute = var2impute, 
                             categoryVar = categoryVar) )




# # save imputed data table
# write.csv(df_imputed,
#           file.path(dataPath, 
#                     "National_Pulse_imported_imputed_week25.csv" ),
#           row.names = F)
# 
# 
# df_imputed_to24 <- read.csv("C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Data/National_PulseData_Paper1/National_Pulse_imported_imputed_1_24.csv",
#                             stringsAsFactors = FALSE)
# df_imputed_25 <- read.csv("C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Data/National_PulseData_Paper1/National_Pulse_imported_imputed_week25.csv",
#                             stringsAsFactors = FALSE)
# 
# df_imputed_all <- df_imputed_to24 %>%
#   bind_rows(df_imputed_25)
# 
write.csv(df_imputed,
          file.path(dataPath,
                    fileNameOut ),
          row.names = F)

