#EEG Data Analysis - Escitalopram and Neuroplasticity Project, Zsido & Molloy et al. 2021
#Code by Eoin N Molloy & Rachel G. Zsido
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
###############################################################################################################################################
#1 set directory
setwd('/Users/Eoin/Documents/EEG/')
###############################################################################################################################################
#2 Set factors:
data_eeg <-read.csv("EEG_Detrended.csv", header = T)
data_eeg$day = factor(data_eeg$Day)
data_eeg$group = factor(data_eeg$Group)
str(data_eeg)

#Slope Model fitting:
#A Intercept Only
Slope_Intercept <- lmer(Slope ~  (1|Subject), data=data_eeg, REML = F)
summary(Slope_Intercept)
#B With Day
Slope_Time <- lmer(Slope ~  day + (1|Subject), data=data_eeg, REML = F)
summary (Slope_Time)
#C Compare A and B
anova(Slope_Intercept, Slope_Time)
#D With both main effects of group and day
Slope_Both <- lmer(Slope ~  day + group + (1|Subject), data=data_eeg, REML = F)
summary (Slope_Both)
#E Compare B and D
anova(Slope_Time, Slope_Both)
#F With both main effects in interaction
Slope_Interaction <- lmer(Slope ~ group*day + (1|Subject), data = data_eeg, REML = F)
summary(Slope_Interaction)
#G Compare D with F
anova(Slope_Both, Slope_Interaction)
#H Anova on full model
anova(Slope_Interaction)
summary(Slope_Interaction)
Anova(Slope_Interaction)
#I Marginal R squared for fixed effects
Slope_Intercept <- lmer(Slope ~  (1|Subject), data=data_eeg, REML = F)
Slope_null <- lmer(Slope ~ day + (1|Subject), data = data_eeg, REML = F)
Slope_null1 <- lmer(Slope ~ group + day + (1|Subject), data = data_eeg, REML = F)
Slope <- lmer(Slope ~ group*day + (1|Subject), data = data_eeg, REML = F)
r.squaredGLMM(Slope_Intercept)
r.squaredGLMM(Slope_null)
r.squaredGLMM(Slope_null1) 
r.squaredGLMM(Slope) 

#Power_10 Model fitting:
#A Intercept Only
Power_10_Intercept <- lmer(Power_10 ~  (1|Subject), data=data_eeg, REML = F)
summary(Power_10_Intercept)
#B With Day
Power_10_Time <- lmer(Power_10 ~  day + (1|Subject), data=data_eeg, REML = F)
summary (Power_10_Time)
#C Compare A and B
anova(Power_10_Intercept, Power_10_Time)
#D With both main effects of group and day
Power_10_Both <- lmer(Power_10 ~  day + group + (1|Subject), data=data_eeg, REML = F)
summary (Power_10_Both)
#E Compare B and D
anova(Power_10_Time, Power_10_Both)
#F With both main effects in interaction
Power_10_Interaction <- lmer(Power_10 ~ group*day + (1|Subject), data = data_eeg, REML = F)
summary(Power_10_Interaction)
#G Compare D with F
anova(Power_10_Both, Power_10_Interaction)
#H Anova on full model
anova(Power_10_Interaction)
summary(Power_10_Interaction)
Anova(Power_10_Interaction)
#I Marginal R squared for fixed effects
Power_10_Intercept <- lmer(Power_10 ~  (1|Subject), data=data_eeg, REML = F)
Power_10_null <- lmer(Power_10 ~ day + (1|Subject), data = data_eeg, REML = F)
Power_10_null1 <- lmer(Power_10 ~ group + day + (1|Subject), data = data_eeg, REML = F)
Power_10 <- lmer(Power_10 ~ group*day + (1|Subject), data = data_eeg, REML = F)
r.squaredGLMM(Power_10_Intercept)
r.squaredGLMM(Power_10_null)
r.squaredGLMM(Power_10_null1) 
r.squaredGLMM(Power_10) 
###############################################################################################################################################
#T-Tests on mean values at both D1 and D7 for each measure:
means <-read.csv("EEG_Detrended_Day_Means.csv", header = T)

#Power 2-sample tests:
t.test(Power_10_Base ~ Group, data = means, var.equal = FALSE)
t.test(Power_10_Day1 ~ Group, data = means, var.equal = FALSE)
t.test(Power_10_Day7 ~ Group, data = means, var.equal = FALSE)

#Slopw 2-sample tests:
t.test(Slope_Base ~ Group, data = means, var.equal = FALSE)
t.test(Slope_Day1 ~ Group, data = means, var.equal = FALSE)
t.test(Slope_Day7 ~ Group, data = means, var.equal = FALSE)
##############################################################################################################################################
#Summary:
#1 - Mixed effects:
#Slope        ----> Significant time effect p<0.001, significant group effect p = 0.03 & significant interaction p<0.001. ***
#Power_10_Log ----> No significant group, time orinteraction effects

#2 - Post-hoc 2-sample ttests on mean values at BL, D1 & D7:
#Power     ----> No significant different at any time point with mean values (BL, D1, D7)
#Slope     ----> No significant different at BL or D7. Sign group difference at D1 (t=-3.3062, p<0.001)
###############################################################################################################################################

###############################################################################################################################################
#Analyses on classic, undetrended data
#2 Set factors:
data_class <-read.csv("Alpha_Classic_Final.csv", header = T)
data_class$day = factor(data_class$Day)
data_class$group = factor(data_class$Group)
str(data_class)

#Relative Power Log10 transformed
#A Intercept Only
Power_Rel_Log_Intercept <- lmer(Power_Rel_Log ~  (1|ID), data=data_class, REML = F)
summary(Power_Rel_Log_Intercept)
#B With Day
Power_Rel_Log_Time <- lmer(Power_Rel_Log ~  day + (1|ID), data=data_class, REML = F)
summary (Power_Rel_Log_Time)
Anova(Power_Rel_Log_Time)
#C Compare A and B
anova(Power_Rel_Log_Intercept, Power_Rel_Log_Time)
#D With both main effects of group and day
Power_Rel_Log_Both <- lmer(Power_Rel_Log ~  day + group + (1|ID), data=data_class, REML = F)
summary (Power_Rel_Log_Both)
Anova(Power_Rel_Log_Both)
#E Compare B and D
anova(Power_Rel_Log_Time, Power_Rel_Log_Both)
#F With both main effects in interaction
Power_Rel_Log_Interaction <- lmer(Power_Rel_Log ~ group*day + (1|ID), data = data_class, REML = F)
summary(Power_Rel_Log_Interaction)
Anova(Power_Rel_Log_Interaction)
#G Compare D with F
anova(Power_Rel_Log_Both, Power_Rel_Log_Interaction)
#H Anova on full model
anova(Power_Rel_Log_Interaction)
summary(Power_Rel_Log_Interaction)
Anova(Power_Rel_Log_Interaction)
#I Marginal R squared for fixed effects
Power_Rel_Log_Intercept <- lmer(Power_Rel_Log ~  (1|ID), data=data_class, REML = F)
Power_Rel_Log_null <- lmer(Power_Rel_Log ~ day + (1|ID), data = data_class, REML = F)
Power_Rel_Log_null1 <- lmer(Power_Rel_Log ~ group + day + (1|ID), data = data_class, REML = F)
Power_Rel_Log <- lmer(Power_Rel_Log ~ group*day + (1|ID), data = data_class, REML = F)
r.squaredGLMM(Power_Rel_Log_Intercept)
r.squaredGLMM(Power_Rel_Log_null)
r.squaredGLMM(Power_Rel_Log_null1) 
r.squaredGLMM(Power_Rel_Log)
#########################################################################################
#T-Tests on mean values at both D1 and D7 for each measure:
means_classic <-read.csv("Means_Classic.csv", header = T)
#relative power first:
#BL
t.test(Power_base  ~ Group, paired=FALSE, var.equal=FALSE, data=means_classic) #not sig
#D1
t.test(Power_day1  ~ Group, paired=FALSE, var.equal=FALSE, data=means_classic) #sig
#D7
t.test(Power_day7  ~ Group, paired=FALSE, var.equal=FALSE, data=means_classic) #not sig
##############################################################################################################################################
#Summary:
#1 - Mixed effects:
#Power_10_Log ----> significant group (p=0.03), time (p<0.001) and interaction (p<0.001)

#2 - Post-hoc 2-sample ttests on mean values at BL, D1 & D7:
#Power_10_Log ----> No significant different at BL or D7, sig group difference at D1 (t=2.3089, p = 0.02)
###############################################################################################################################################
#Linear Regression prediction analyses
reg <-read.csv("residuals.csv", header = T)
#Base vs Day1
linearMod_1 <- lm(Base_ResD7 ~ Day1_ResD7, data=reg)
summary(linearMod_1)
#Base vs Day7
linearMod_2 <- lm(Base_ResD1 ~ Day7_ResD1, data=reg)
summary(linearMod_2)
#Day1 vs Day 7
linearMod_3 <- lm(Day_1_ResBL ~ Day_7_ResBL, data=reg)
summary(linearMod_3)
###############################################################################################################################################
demo <- read.csv("ASEC_STAI.csv", header = T)
demo$day = factor(demo$Day)
demo$group = factor(demo$Group)
str(demo)
#ASEC ANOVA
asec <- subset(demo, Day != "3", rename=c())
a <- lmer(ASEC ~ group * day + (1|ID), data=asec)
anova(a)
#ASEC TTests
ttest <- read.csv("STAI_ASEC_Plasma_Mean_Brain.csv", header = T)
t.test(ASEC_D1  ~ Group, paired=FALSE, var.equal=FALSE, data=ttest) #sig
t.test(ASEC_D7  ~ Group, paired=FALSE, var.equal=FALSE, data=ttest) #not sig
#STAI ANOVA
b <- lmer(STAI ~ group * day + (1|ID), data=demo)
anova(b)
###############################################################################################################################################
#Correlations - Escitalopram Group
cors <- read.csv("STAI_ASEC_Plasma_Mean_Brain.csv", header = T)
cors <- subset(cors, Group != "Placebo", rename=c())
#ASEC Plasma
#D1
cor.test(cors$ASEC_D1, cors$Plasma_D1, 
         method = "pearson")
#D7
cor.test(cors$ASEC_D7, cors$Plasma_D7, 
         method = "pearson")

#Power
#D1
cor.test(cors$ASEC_D1, cors$Power_Log10_Day1, 
         method = "pearson")
#D7
cor.test(cors$ASEC_D7, cors$Power_Log10_Day7, 
         method = "pearson")
#Slope
#D1
cor.test(cors$ASEC_D1, cors$Slope_2, 
         method = "pearson")
#D7
cor.test(cors$ASEC_D7, cors$Slope_3, 
         method = "pearson")

#Against brain signal in placebo
cors <- read.csv("STAI_ASEC_Plasma_Mean_Brain.csv", header = T)
cors <- subset(cors, Group != "SSRI", rename=c())
#Power
#D1
cor.test(cors$ASEC_D1, cors$Power_Log10_Day1, 
         method = "pearson")
#D7
cor.test(cors$ASEC_D7, cors$Power_Log10_Day7, 
         method = "pearson")
#Slope
#D1
cor.test(cors$ASEC_D1, cors$Slope_2, 
         method = "pearson")
#D7
cor.test(cors$ASEC_D7, cors$Slope_3, 
         method = "pearson")

#POMs Correlations:
#Correlations - Escitalopram Group
cors <- read.csv("AST.csv", header = T)
cors <- subset(cors, Group != "Placebo", rename=c())
#Slope
#BL
cor.test(cors$CMD_BL, cors$Slope_1, 
         method = "pearson")
#D1
cor.test(cors$CMD_D1, cors$Slope_2, 
         method = "pearson")
#D7
cor.test(cors$CMD_D7, cors$Slope_3, 
         method = "pearson")

#Power
#BL
cor.test(cors$CMD_BL, cors$Power_1, 
         method = "pearson")
#D1
cor.test(cors$CMD_D1, cors$Power_2, 
         method = "pearson")
#D7
cor.test(cors$CMD_D7, cors$Power_3, 
         method = "pearson")
###############################################################################################################################################
#ESS ANOVA
sleepy <- read.csv("ESS.csv", header = T)
sleepy$day = factor(sleepy$Day)
sleepy$group = factor(sleepy$Group)
str(sleepy)
ess <- lmer(ESS ~ group * day + (1|ID), data=sleepy)
anova(ess)

poms <-read.csv("POMS.csv", header = T)
poms$day = factor(poms$Day)
poms$group = factor(poms$Group)
str(poms)

poms2<-subset(poms, Subject != "Sub65" & Subject !="Sub72"  & Subject != "Sub80" &  Subject!="Sub4",rename=c())
results.poms2 <- lme(CMD ~ day * group, random=~1|Subject, data=poms2)
anova(results.poms2) #time no group, no interaction
#Compare another method:
poms2 <- lmer(CMD ~ group * day + (1|Subject), data=poms2)
anova(poms2)#time no group, no interaction
###############################################################################################################################################
