
rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts_ILE"
setwd(path_wd)

library(tidyverse)
# library(purrr)
library(RColorBrewer)
source("../Scripts/pulse_process_functions.R")

# -------------------------------------------------------------------------

# data path
dataPath <- "../Data/"

importDataPath <- file.path(dataPath, "National_PulseData_ILE")



# fileNameBase <- "National_Pulse_FoodMoneyInsuf_EligibleIncomeWorkingAge_byEIP_RECV"
# fileNameBase <- "National_Pulse_AnxDepFoodMoney_EligibleIncomeWorkingAge_byEIP_RECV"
# fileNameBase <- "National_Pulse_BehindRentImputed_EligibleIncomeWorkingAge_byEIP_RECV"
# fileNameBase <- "National_Pulse_AnxDep_EligibleIncomeWorkingAge_byEIP_RECV"

fileNameBase <- "National_Pulse_wtPrev_EligibleIncomeWorkingAgeGender_byEIP_RECV"
stratVar <- NULL
hetStratVar <- NULL
plot_width <- 6 # 4(1), 6(2), 8(3), 10(4), 12(5)
plot_height <- 5 # 5(1), 6(2),

plotTitle <- ""
# plotTitle <- "Anxiety and Depression Symptoms\nWorking Age & Eligible Income"
# plotTitle <- "Food and Money Insufficiency\nWorking Age & Eligible Income"
# plotTitle <- "Difficulty Paying for Expenses\nWorking Age & Eligible Income"
# plotTitle <- "Behind on Rent\nWorking Age Renters & Eligible Income"


strat_variables <- "EIP_RECV"
levels_for_stratvar <- c("NoEIP","EIP","Baseline")
labels_for_stratvar <- c("Did not receive stimulus payment\nin past 7 days",
                         "Received stimulus payment\nin past 7 days",
                         "Baseline (pre-stimulus payment)")




# -------------------------------------------------------------------------

df_prev <- read.csv(file.path(importDataPath, paste0(fileNameBase,".csv"))) %>%
  mutate(midDate = as.Date(midDate, format = "%m/%d/%Y"),
         startDate = as.Date(startDate, format = "%m/%d/%Y"),
         endDate = as.Date(endDate, format = "%m/%d/%Y" ) ) %>%
  # mutate(midDate = as.Date(midDate, format = "%Y-%m-%d"),
  #        startDate = as.Date(startDate, format = "%Y-%m-%d"),
  #        endDate = as.Date(endDate, format = "%Y-%m-%d" ) ) %>%
  filter(WEEK < 23 & Phase == 3) %>%
  mutate(EIP_RECV = factor(EIP_RECV, 
                           levels = levels_for_stratvar,
                           labels = labels_for_stratvar) ) %>%
  # label string
  mutate(dateRangeStr = paste0( format( startDate ,  "%m/%d") , " - ",
                                format( endDate ,  "%m/%d/%y")  ) ) 






# -------------------------------------------------------------------------

# EIP plots
# pos_d <- position_dodge( width=3 )



pos_d <- position_dodge( width=1 )

df_prev_22tmp <- df_prev %>%
  filter(WEEK == 22) %>%
  mutate(WEEK = 23)
df_prev_18_21 <- df_prev %>%
  filter(WEEK %in% c(18:21))


width_bar <- 0.4
pos_d <- position_dodge( width=width_bar*1.1 )


p2 <- df_prev_22tmp %>%
  # filter(Outcome %in% c("Anxiety Symptoms", "Depression Symptoms")) %>%
  filter(Outcome %in% c("Difficulty with Expenses", "Food Insufficient")) %>%
  ggplot(aes(x = Outcome, y = prop, fill = EIP_RECV, group = EIP_RECV) ) +
  # geom_vline(xintercept = 22) +
  geom_bar(position = pos_d, stat = "identity", width =  width_bar) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u, width = 0),position = pos_d, width = .2) +
  
  theme_bw() +
  # scale_x_continuous(breaks = c(18,19,20,21,23),
  #                    minor_breaks = NULL,
  #                    labels = c(unique(df_prev_18_21$dateRangeStr), unique(df_prev_22tmp$dateRangeStr))
  #                    # limits = c(17.7, 24)
  # )+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust=1)
  )+
  guides(fill=guide_legend(ncol=1, order = 2)) +
  scale_fill_brewer(palette = "Dark2", direction = -1)+
  xlab("") +
  ylab("Prevalence, % (95% CI)") +
  ggtitle(plotTitle) +
  ylim(0, NA)


p2

ggsave(file.path(importDataPath,
                 paste0(fileNameBase, stratVar, "_.png")),
       plot = p2,
       device = "png",
       width = plot_width,
       height = plot_height)





















# -------------------------------------------------------------------------
# plot weeks 18-22
# -------------------------------------------------------------------------


p1 <- df_prev_22tmp %>%
  ggplot(aes(x = WEEK, y = prop, fill = EIP_RECV, group = EIP_RECV) ) +
  # geom_vline(xintercept = 22) +
  geom_bar(position = pos_d, stat = "identity") +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u, width = 0),position = pos_d, width = .2) +
  geom_bar(data = df_prev_18_21, position = pos_d, stat = "identity", width = 0.45) +
  geom_errorbar(data = df_prev_18_21, aes(ymin = ci_l, ymax = ci_u, width = 0), position = pos_d, width = .1) +
  
  theme_bw() +
  scale_x_continuous(breaks = c(18,19,20,21,23),
                     minor_breaks = NULL,
                     labels = c(unique(df_prev_18_21$dateRangeStr), unique(df_prev_22tmp$dateRangeStr))
                     # limits = c(17.7, 24)
                     )+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust=1)
  )+
  guides(fill=guide_legend(ncol=1, order = 2)) +
  scale_fill_brewer(palette = "Dark2")+
  xlab("") +
  ylab("Prevalence, % (95% CI)") +
  ggtitle(plotTitle) +
  ylim(0, NA)

if ( is.null(hetStratVar) )
{
  p2 <- p1 + facet_wrap(vars(Outcome), nrow = 1, scales = "free_y")  +theme(panel.spacing = unit(2, "lines"))
  
} else {
  p2 <- p1 + facet_grid(cols = vars(!!sym(hetStratVar)), rows = vars(Outcome)) 
}

p2

ggsave(file.path(importDataPath,
                 paste0(fileNameBase, stratVar, "_.png")),
       plot = p2,
       device = "png",
       width = plot_width,
       height = plot_height)

# # 5
# width = 12,
# height = 6)

# # 3
# width = 8,
# height = 6)

# # 1
# width = 4,
# height = 6)

# # 2
# width = 6,
# height = 6)


  


