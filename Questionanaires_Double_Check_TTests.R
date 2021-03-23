#EEG Data Analysis - Escitalopram and Neuroplasticity Project, Zsido & Molloy et al.
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
install.packages("ggpubr")
###############################################################################################################################################
#1 set directory
setwd('/Users/eoin/Dropbox/My_Stuff/3_EEG/Items')

cors <- read.csv("STAI_ASEC_Plasma_Mean_Brain.csv", header = T)
t.test(STAI_D1  ~ Group, paired=FALSE, var.equal=FALSE, data=cors)
t.test(STAI_D7  ~ Group, paired=FALSE, var.equal=FALSE, data=cors)

poms <-read.csv("POMS_Wide.csv", header = T)
t.test(CMD_Day1  ~ Group, paired=FALSE, var.equal=FALSE, data=poms)
t.test(CMD_Day7  ~ Group, paired=FALSE, var.equal=FALSE, data=poms)

ttest_ESS <- read.csv("ESS_t_test.csv", header = T)
t.test(ESS_1  ~ Group, paired=FALSE, var.equal=FALSE, data=ttest_ESS)
t.test(ESS_7  ~ Group, paired=FALSE, var.equal=FALSE, data=ttest_ESS)
