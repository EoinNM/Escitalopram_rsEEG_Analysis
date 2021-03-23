###############################################################################################################################################
#0 Load necessary packages
library(magrittr)
library(dplyr)
library(rmcorr)
###############################################################################################################################################
#1 set directory
setwd('/Users/Eoin/Documents/EEG/')
###############################################################################################################################################
#2 Days refers to EEG - D1 to D7 is for fMRI in both analyises
data_eeg <-read.csv("PPI_Cluster_EEG_Correlation_BL_D7.csv", header = T)
#2A Correlate D1 to D7 in both fMRUI and EEG
multimodal_corr_D1_D7 = rmcorr(participant=Subject, measure1=PPI_Cluster, 
                          measure2=EEG_Cluster, dataset=data_eeg, CIs=c("analytic", "bootstrap"), nreps = 100, bstrap.out = F) #not significant
#2B Correlate D1 to D67 fMRI with BL to D7 in EEG
multimodal_corr_BL_D7 = rmcorr(participant=Subject, measure1=BOLD, 
                         measure2=Slope_BL_D7, dataset=data_eeg, CIs=c("analytic", "bootstrap"), nreps = 100, bstrap.out = F) #not significant

