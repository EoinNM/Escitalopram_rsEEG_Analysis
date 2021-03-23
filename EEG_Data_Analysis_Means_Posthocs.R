#Code by ENM
###############################################################################################################################################
#0 Load necessary packages
library(magrittr)
library(dplyr)
library(lme4)
library(car)
library(lmerTest)
library(psych)
require(MuMIn)
library(ggpubr)
library(rmcorr)
###############################################################################################################################################
#1 set directory
setwd('/Users/Eoin/Documents/EEG/')
means <-read.csv("EEG_Detrended_Day_Means.csv", header = T)
#Between groups:
t.test(Slope_Day1 ~ Group, data = means, var.equal = FALSE) #Significant
t.test(Slope_Day7 ~ Group, data = means, var.equal = FALSE) #Not significant
#Within groups - esitalopram
SSRI<-subset(means, Group != "Placebo",rename=c())
t.test(SSRI$Slope_Base, SSRI$Slope_Day7, paired = TRUE, alternative = "two.sided") #Significant
t.test(SSRI$Slope_Base, SSRI$Slope_Day1, paired = TRUE, alternative = "two.sided") #Significant
t.test(SSRI$Slope_Day1, SSRI$Slope_Day7, paired = TRUE, alternative = "two.sided") #Significant
#Within groups - placebo
PLAC<-subset(means, Group != "Escitalopram",rename=c())
t.test(PLAC$Slope_Base, PLAC$Slope_Day7, paired = TRUE, alternative = "two.sided") #Not significant
t.test(PLAC$Slope_Base, PLAC$Slope_Day1, paired = TRUE, alternative = "two.sided") #Not significant
t.test(PLAC$Slope_Day1, PLAC$Slope_Day7, paired = TRUE, alternative = "two.sided") #Not significant