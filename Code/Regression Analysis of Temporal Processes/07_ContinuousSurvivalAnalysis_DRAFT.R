#===========================================================#
#====== CONTINUOUS SURVIVAL ANALYSIS & COX REGRESSION ======#
#===========================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory 
setwd("INSERT PATH TO DIRECTORY") 

# Load packages
library(QMSS)
library(ggplot2)
library(plyr)
library(psych)


# load GSS panel data 
load("GSS_panel.RData")





# Continuous Survival Analysis --------------------------------------------
# _________________________________________________________________________

### What factors affect someone’s probability of becoming employed full-time? ###

# Recodes

vars <- c("idnum", "panelwave", "sex", "age", "educ", "race", "wrkstat")
sub <- pd[, vars]
sub$fulltime <- ifelse(sub$wrkstat == 1, 1, 0)

# I only want to have people who are not fulltime employed at Wave 1, so I drop
# all the people who were fulltime at Wave 1
exclude <- with(sub, which(panelwave==1 & fulltime==1))
sub <- sub[-exclude, ]

# Now, I still have the other years of data for those people who were employed
# fulltime at Wave 1 ... but I don’t care about them at all, so I look for
# people who start now at Wave 2 or Wave 3 (b/c I eliminated them in Wave 1)
sub <- arrange(sub, idnum)
sub <- ddply(sub, "idnum", mutate, min.panel = min(panelwave))
exclude <- with(sub, which(min.panel %in% c(2,3) | panelwave==1))
sub <- sub[-exclude, ]




# Hazard rate: What percent of people went from not full-time employed to
# full-time employed over each wave?
ddply(sub, "panelwave", summarise, mean = mean(fulltime, na.rm = T))
with(sub, TabX(fulltime, panelwave))

# Cox proportional hazards model
# Is this correct? do I need cluster(idnum)??
library(survival)
Surv <- with(sub, Surv(time = panelwave, event = fulltime))
coxph(Surv ~ age + educ + sex + factor(race), data = sub)

  
# Compare coefficients when using different methods for breaking ties
Coefs.ties <- mat.or.vec(nr = 5, nc = 3)
colnames(Coefs.ties) <- c("efron","breslow","exact") # exact = exact partial likelihood
for(i in 1:3){
  coef <- coef( coxph(Surv ~ age + educ + sex + factor(race), data = sub, 
                      method = colnames(Coefs.ties)[i]) )
  Coefs.ties[,i] <- coef(fit)
}
rownames(Coefs.ties) <- names(coef)
print(Coefs.ties, digits = 3)


# Compare coefficients & std errors for model with all ages and model with 30-60 yr olds
Coefs.ages <- mat.or.vec(nr = 10, nc = 2) 
colnames(Coefs.ages) <- c("all","restricted")
fit1 <- coxph(Surv ~ age + educ + sex + factor(race) + cluster(idnum), data = sub)
fit2 <- update(fit1, subset = age %in% 30:60) 
Coefs.ages[,"all"] <- t(summary(fit1)$coef[,c("coef","robust se")])
Coefs.ages[,"restricted"] <- t(summary(fit2)$coef[,c("coef","robust se")])
rownames(Coefs.ages) <- paste0(c("b_", " se_"), rep(rownames(Coefs.ties),each=2))
print(Coefs.ages, digits = 3)


# Testing proportionality assumption 
cox.fulltime <- coxph(Surv ~ age + factor(sex) + factor(race) + cluster(idnum), data = sub)
cox.zph(cox.fulltime) 

# The survival curves of women vs. men
by.sex <- survfit(Surv ~ factor(sex), data = sub, id = idnum, type = "kaplan-meier")
plot(by.sex, col = c("skyblue", "maroon"), lty = c(1,2), lwd = 2)
legend("bottom", c("Males", "Females"), horiz = T, bty = "n",
       col = c("skyblue", "maroon"), lty = c(1,2), lwd = 2)

# The log-rank test
survdiff(Surv ~ factor(sex), data = sub)

