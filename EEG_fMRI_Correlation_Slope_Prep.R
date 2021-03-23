###############################################################################################################################################
#0 Load necessary packages
library(magrittr)
library(dplyr)
library(rmcorr)
###############################################################################################################################################
#1 set directory
setwd('/Users/Eoin/Documents/EEG/')
###############################################################################################################################################
#2 Set factors:
data_eeg <-read.csv("EEG_Detrended.csv", header = T)
prem <- subset(data_eeg, Chan_Name == "F3" | Chan_Name == "FC3" | Chan_Name == "FT7" | Chan_Name == "T7", rename=c())

prem <- prem %>%
  group_by(Subject) %>%
  mutate(Slope_Prem_Base = mean(c(Slope[Day== "1"]), na.rm=TRUE))
prem <- prem %>%
  group_by(Subject) %>%
  mutate(Slope_Prem_Day1 = mean(c(Slope[Day== "2"]), na.rm=TRUE))
prem <- prem %>%
  group_by(Subject) %>%
  mutate(Slope_Prem_Day7 = mean(c(Slope[Day== "3"]), na.rm=TRUE))

write.csv(prem, "EEG_Premotor_BL_D1_D7.csv")
