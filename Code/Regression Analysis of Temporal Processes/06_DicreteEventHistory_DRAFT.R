# To do:
# ask greg about STATA recodes

#=============================================#
#====== DISCRETE EVENT HISTORY ANALYSIS ======#
#=============================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/21/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load packages
library(QMSS)
library(ggplot2)
library(plyr)
library(psych)



# load GSS panel data 
load("GSS_panel.RData")



# One-time events ---------------------------------------------------------
# _________________________________________________________________________

### What factors affect someone’s probability of becoming employed full-time? ###

vars <- c("idnum", "panelwave", "sex", "age", "educ", "race", "wrkstat")
sub <- pd[, vars]
sub$fulltime <- ifelse(sub$wrkstat == 1, 1, 0)

# The raw risks
?TabX
with(sub, TabX(fulltime, panelwave))

# Reshape to wide
sub.wide <- reshape(sub, direction = "wide", 
                    idvar = "idnum", 
                    timevar = "panelwave", 
                    drop = "wrkstat")

# The conditional risks in Wave 2
with(sub.wide, Tab(fulltime.2[fulltime.1 == 0]))

# The conditional risks in Wave 3
with(sub.wide, Tab(fulltime.3[fulltime.2 == 0]))

# Drop all people who were fulltime at Wave 1
exclude <- with(sub, which(panelwave==1 & fulltime==1))
sub2 <- sub[-exclude, ]



# look for people who start now at Wave 2 or Wave 3 and drop all those extra
# observations from the people who were fulltime employed at Wave 1
sub2 <- arrange(sub2, idnum)
sub2 <- ddply(sub2, "idnum", mutate, min.panel = min(panelwave))
exclude <- with(sub2, which(min.panel %in% c(2,3) | panelwave==1))
sub2 <- sub2[-exclude, ]


# generate lag of fulltime
library(Hmisc)
sub2 <- ddply(sub2, "idnum", mutate, l.fulltime = Lag(fulltime))
# sub2$l.fulltime <- lag(sub2$fulltime)

# now take difference of current fulltime vs. previous fulltime score
sub2$diff_ft_min_lft <- with(sub2, fulltime - l.fulltime)

# Change the years when someone went from fulltime to not full time to missing
# b/c that is like going from dead to undead, which is not allowed yet
sub2$fulltime[sub2$diff_ft_min_lft == -1] <- NA

# I change the years after someone first goes from being not-fulltime to 
# fulltime to missing, since you can only get a score of 1 on fulltime once ... 
# after that, it is like being dead. 
sub2 <- ddply(sub2, "idnum", mutate, sumft = sum(fulltime, na.rm = T))

# Keep sub2 as it is now and make a copy called called sub_1event. 
sub_1event <- sub2
sub_1event$fulltime[with(sub_1event, sumft == 2 & panelwave == 3)] <- NA

# I will want to cut my sample according to age, but I need to know the maximum
# age that same person is across the 3 Waves, so I generate a maximum age
# variable – that way, I can exclude all observations from the same person, not
# just when they over a certain age in a certain year.
sub_1event <- ddply(sub_1event, "idnum", mutate, maxage = max(age, na.rm = T))


### The simplest model ###
library(lme4)
simple.model <- glmer(fulltime ~ factor(panelwave) + (1 | idnum), family = binomial, data = sub_1event)
summary(simple.model)

### A better model ###
better.model <- glmer(fulltime ~ age + educ + sex + factor(race) + factor(panelwave) + (1 | idnum), 
                      family = binomial, data = sub_1event)
summary(better.model)

### Compare to regular logit ###
# using lrm from rms package so we can use idnum as cluster variable 
logit.model <- lrm(fulltime ~ age + educ + sex + factor(race) + factor(panelwave), 
                   data = sub_1event, x = T, y = T)
robcov(logit.model, cluster = sub_1event$idnum)

### Only for the fully employable ###
better.model2 <- glmer(fulltime ~ age + educ + sex + factor(race) + factor(panelwave) + (1 | idnum), 
                      family = binomial, 
                      data = subset(sub_1event, maxage %in% 26:59)) # or maxage > 25 & maxage < 60
summary(better.model2)

### What would have happened if I didn’t do this as an event history analysis? ###

summary(glmer(fulltime ~ age + educ + sex + factor(race) + factor(panelwave) + (1 | idnum), 
              family = binomial, 
              data = sub)) # use original subset without all the modifications we wade





# Repeated events ---------------------------------------------------------
# _________________________________________________________________________

### A slightly different question ###
# What factors affect someone’s probability of becoming (and staying) employed
# full-time, given they weren’t employed full-time initially?

sub_Nevents <- sub2

# FIGURE OUT THIS RECODE????
good <- with(sub_Nevents, (sumft == 2 | diff_ft_min_lft ==-1 ) & panelwave==3)
sub_Nevents$yrsft <- ifelse(good, 1, 0)

multEvents.model <- glmer(fulltime ~ I(scale(age)) + I(scale(educ)) + I(scale(sex)) + factor(race) 
                          + factor(panelwave) + yrsft + (1 | idnum), 
                          family = binomial, 
                          data = sub_Nevents,
                          control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10^5)),
                          nAGQ = 10)
print(multEvents.model, corr = FALSE)




# Multiple outcomes ("competing risks" model) -----------------------------
# _________________________________________________________________________

### Now, a slightly different question again ###
# What factors affect someone’s probability of becoming (and staying) employed
# full-time, vs. being part-time employed, vs. being not employed at all?

sub$work <- mapvalues(sub$wrkstat, from = 1:8, to = c(1,2,rep(3,6)) )
