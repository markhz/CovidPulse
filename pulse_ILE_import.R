
rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts_ILE"
setwd(path_wd)

library(tidyverse)
library(purrr)
library(mice)
source("../Scripts/pulse_process_functions.R")

# -------------------------------------------------------------------------

# data path
dataPath <- "../Data/"
rawDataPath <- file.path(dataPath, "PUF_CSV")

outputPath <- file.path(dataPath, "National_PulseData_ILE")
filePath_var_OI <- file.path(outputPath, "var_OI_ILE.csv")

# -------------------------------------------------------------------------

dir.create(outputPath, showWarnings = FALSE)

df_states <- read.csv(file.path(dataPath, "pulse_states.csv")) %>%
  mutate(REGION = as.factor(REGION))

# import table indicating variables to import
var_OI <- import_clean_var_OI(filePath_var_OI)

# -------------------------------------------------------------------------
# import all survey data, filter and merge
# -------------------------------------------------------------------------

# get paths to each survey
fullFilePaths_tmp <- getFilePaths(rawDataPath)

# only import select Weeks
fullFilePaths <- fullFilePaths_tmp[13:length(fullFilePaths_tmp) ]


# -------------------------------------------------------------------------

# import data
start_time = Sys.time()

df_import <- fullFilePaths %>%
  map_df(~ importPulseData(.x, var_OI ))

end_time = Sys.time()
time_import <- end_time - start_time

# -------------------------------------------------------------------------

# add State and Region name variables
df_import <- df_import %>%
  left_join(df_states, by = c("EST_ST" = "StateValue"))


# -------------------------------------------------------------------------

# save data frame with variables of interest
write.csv(df_import,
          file.path(outputPath, "National_Pulse_imported.csv"),
          row.names = F)




  


