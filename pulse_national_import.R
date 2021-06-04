
rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts"
setwd(path_wd)

library(tidyverse)
library(purrr)
source("pulse_process_functions.R")

# -------------------------------------------------------------------------

# data path
dataPath <- "../Data/"
rawDataPath <- file.path(dataPath, "PUF_CSV")

outputPath <- file.path(dataPath, "National_PulseData_Paper1")
filePath_var_OI <- file.path(outputPath, "var_OI_paper1.csv")

# outputPath <- file.path(dataPath, "National_PulseData_ILE")
# filePath_var_OI <- file.path(outputPath, "var_OI_ILE.csv")


# -------------------------------------------------------------------------

dir.create(outputPath, showWarnings = FALSE)

df_states <- read.csv(file.path(dataPath, "pulse_states.csv")) %>%
  mutate(REGION = as.factor(REGION))

# import table indicating variables to import
var_OI <- import_clean_var_OI(filePath_var_OI)
# var_OI <- c("SCRAM", "WEEK", "RSNNOWRK")
# -------------------------------------------------------------------------
# import all survey data, filter and merge
# -------------------------------------------------------------------------

# get paths to each survey
fullFilePaths <- getFilePaths(rawDataPath)


# -------------------------------------------------------------------------

# import data
start_time <- Sys.time()

df_import <- fullFilePaths %>%
  map_df(~ importPulseData(.x, var_OI ))

end_time <- Sys.time()
end_time - start_time

# df_import_add <- df_import
# df_import_ <- read.csv(file.path(outputPath, "National_Pulse_imported_imputed_no_rsnnowork.csv"))
# 
# df_import <- df_import_ %>% left_join(df_import_add, by = c("SCRAM", "WEEK"))
# 
# df_import <- df_import %>% select( names(df_import)[c(1:23, 27, 24:26) ] )

# -------------------------------------------------------------------------

# add State and Region name variables
df_import <- df_import %>%
  left_join(df_states, by = c("EST_ST" = "StateValue"))


# -------------------------------------------------------------------------

# save data frame with variables of interest
# write.csv(df_import,
#           file.path(outputPath, "National_Pulse_imported_imputed.csv"),
#           row.names = F)
write.csv(df_import,
          file.path(outputPath, "National_Pulse_imported.csv"),
          row.names = F)



