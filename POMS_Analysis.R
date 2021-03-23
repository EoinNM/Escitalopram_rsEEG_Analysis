#########################################################################################
library(nlme)
#########################################################################################
#1 set directory
setwd('/Users/Eoin/Documents/EEG/')
poms <-read.csv("POMS.csv", header = T)
poms$day = factor(poms$Day)
poms$group = factor(poms$Group)
str(poms)

results.poms <- lme(CMD ~ day * group, random=~1|Subject, data=poms)
anova(results.poms) #time no group, no interaction

poms2<-subset(poms, Subject != "PJO065" & Subject !="NGR072"  & Subject != "MCA080" &  Subject!="VMA004",rename=c())
results.poms2 <- lme(CMD ~ day * group, random=~1|Subject, data=poms2)
anova(results.poms2) #time no group, no interaction

poms2 <- lmer(CMD ~ group * day + (1|Subject), data=poms2)
anova(poms2)#time no group, no interaction
