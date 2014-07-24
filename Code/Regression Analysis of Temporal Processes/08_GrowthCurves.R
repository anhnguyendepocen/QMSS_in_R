#===========================#
#====== GROWTH CURVES ======#
#===========================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/24/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load packages
library(QMSS)
library(ggplot2)
library(plyr)
library(psych)
library(rms)
library(lme4)

# load GSS panel data 
load("GSS_panel.RData")




 
# Growth curves intro -----------------------------------------------------
# _________________________________________________________________________


vars <- c("idnum", "panelwave", "sex", "age", "educ", "race", "polviews", "confinan")
sub <- pd[, vars]
sub$n.confinan <- ReverseThis(sub$confinan)

# Overall trend in confidence in banks
g_trend <- ggplot(sub, aes(x = panelwave, y = n.confinan))
(g_trend <- g_trend + stat_summary(fun.y=mean, geom="line", lwd = 1.25, color="navyblue"))

# Empirical growth curves for idnum < 200 (& overall)
g_id <- ggplot(subset(sub, idnum<200), aes(x=panelwave, y=n.confinan, group=idnum))
(g_id <- g_id + geom_line(aes(color = factor(idnum))))
(g_id <- g_id + stat_summary(fun.y=mean, geom="line", aes(group=1), lty = 2, color="black"))
g_id + theme(legend.position="none")

# individual regression lines for idnum < 200 (& overall)
g_reg <- ggplot(subset(sub, idnum < 200), aes(x = panelwave, y = n.confinan, group = idnum))
(g_reg <- g_reg + stat_smooth(method = lm, se = F, aes(color = factor(idnum))))
(g_reg <- g_reg + stat_summary(fun.y=mean, geom="smooth", aes(group=1), lty = 2, color = "black"))
g_reg + theme(legend.position="none")

# overall linear prediction
g_lm <- (ggplot(sub, aes(x = panelwave, y = n.confinan)) 
         + stat_summary(fun.y=mean, geom="point", aes(group=1), size=4, color = "navyblue"))
(g_lm <- g_lm + stat_smooth(method = lm, se = F, color = "skyblue", lwd = 1.25))
 
# add quadratic prediction curve
(g_lm <- g_lm + stat_smooth(formula = y ~ poly(x,2), method = lm, se = F, 
                   color = "maroon", lty = 2, lwd = 1.25))


# ols with clustered & robust SEs
robcov(ols(n.confinan ~ factor(panelwave), x = T, y = T, data = sub), 
       cluster = sub$idnum)

# ols with clustered & robust SEs (no separate intercepts)
robcov(ols(n.confinan ~ panelwave, x = T, y = T, data = sub), 
       cluster = sub$idnum)




# Random intercepts -------------------------------------------------------
# _________________________________________________________________________

lmer.confinan <- lmer(n.confinan ~ panelwave + (1|idnum), data = sub, REML = F)
summary(lmer.confinan)
?rho
rho(lmer.confinan)
VarCorr(lmer.confinan)

# random intercept model with quadratic term
lmer.confinan2 <- update(lmer.confinan, ~ . + I(panelwave^2))
summary(lmer.confinan2)
rho(lmer.confinan2)
VarCorr(lmer.confinan2)



# Random intercepts & slopes ----------------------------------------------
# _________________________________________________________________________

lmer.confinan3 <- lmer(n.confinan ~ panelwave + (1 + panelwave | idnum), 
                       data = sub, REML = F)
summary(lmer.confinan3)
VarCorr(lmer.confinan3)

# are random slopes necessary? likelihood ratio test
anova(lmer.confinan, lmer.confinan3)

# add in a time-invariant characteristic
sub$male <- ifelse(sub$sex==1, "male", "female")

lmer.confinan4 <- update(lmer.confinan3, ~ . + male)
summary(lmer.confinan4)

# add in a time-varying characteristic too
lmer.confinan5 <- update(lmer.confinan4, ~ . + polviews)
summary(lmer.confinan5)



# Interactions ------------------------------------------------------------
# _________________________________________________________________________

# changes in confidence in banks, by sex
g_sex <- ggplot(sub, aes(x = panelwave, y = n.confinan, color = male))
(g_sex <- (g_sex + stat_summary(fun.y=mean, geom="line", lwd = 1.25)))

lmer.confinan6 <- update(lmer.confinan5, ~ . + male:panelwave - polviews)
summary(lmer.confinan6)

model.dat <- cbind(model.frame(lmer.confinan6), fitted = fitted(lmer.confinan6))
model.dat <- subset(model.dat, idnum < 200)
g_sex_fit <- ggplot(model.dat, aes(x = panelwave, y = fitted, group = idnum, color = male))
(g_sex_fit <- (g_sex_fit + geom_line() + geom_point() + facet_grid( . ~ male)))



# Random intercepts/random slopes for men
lmer.confinanM <- lmer(n.confinan ~ panelwave + (1 + panelwave | idnum), 
                       data = sub, subset = sex == 1)
summary(lmer.confinanM)

# Random intercepts/random slopes for women
lmer.confinanW <- update(lmer.confinanM, subset = sex == 2)
summary(lmer.confinanW)


