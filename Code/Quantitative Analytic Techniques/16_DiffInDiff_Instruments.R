#===============================================================#
#====== DIFFERENCE IN DIFFERENCES, INSTRUMENTAL VARIABLES ======#
#===============================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# load packages
library(QMSS)
library(memisc)
library(plyr)
library(visreg)

# Load the cumulative GSS dataset 
load("GSS.RData")




# Difference-in-differences -----------------------------------------------
# _________________________________________________________________________


### Does opposite-party president affect Republican happiness more than
### Democratic happiness? 

vars <- c("happy", "partyid", "year", "age", "educ", "sex", 
          "realinc", "polviews", "race", "region")
sub <- GSS[, vars]
Tab(sub$happy)
sub$n.happy <- ReverseThis(sub$happy)
with(sub, table(happy, n.happy))

Tab(sub$partyid)
sub$repub <- mapvalues(sub$partyid,
                      from = 0:7,
                      to = c(rep(0,3), NA, rep(1,3), NA))
Tab(sub$repub)


lm.president <- lm(n.happy ~ as.factor(year)*repub, data = sub, year==2006 | year==2010)
summary(lm.president)


# with controls
lm.president2 <- update(lm.president, ~ . + age + educ + sex + realinc 
                        + polviews + as.factor(race) + as.factor(region))
summary(lm.president2)


# Plot trend of mean happiness score, Republicans vs. Democrats, over time
happy.by.year <- ddply(sub, c("year", "repub"), summarise, mean = mean(n.happy, na.rm = T))
happy.by.year <- na.omit(happy.by.year)
plot_colors <- scale_color_manual(values = c("blue", "red"), name = "repub") 

# linear fit for years 2006 to 2010
g_happy_06to10 <- ggplot(subset(happy.by.year, year %in% 2006:2010), 
                         aes(x = year, y = mean, group = repub, color = factor(repub)))
g_happy_06to10 + plot_colors + stat_smooth(method = "lm", se = F) 

# for all available years, actual trend with linear fit
g_happy_all <- ggplot(happy.by.year, aes(x = year, y = mean, 
                                         group = repub, color = factor(repub)))
g_happy_all + plot_colors + stat_smooth(method = "lm", se = F, lty = 2) + geom_line()




# Instrumental variables --------------------------------------------------
# _________________________________________________________________________

vars <- c("attend", "educ", "paeduc", "year", "age", "relig", "region", "dwelown")
sub <- GSS[, vars]
Tab(sub$attend)
sub$n.attend <- mapvalues(sub$attend,
                         from = 0:8,
                         to = c(0, 0.5, 2, 12, 24, 30, 40, 52, 88))
Tab(sub$n.attend)


# simple OLS model
lm.attend <- lm(n.attend ~ educ + year + age + as.factor(relig) 
                + as.factor(region) + as.factor(dwelown), data = sub)
summary(lm.attend)



# instrumental variable model (using paeduc as instrument)
# install.packages("AER") 
library(AER) 
iv.attend <- ivreg(n.attend ~ 
  # first write equation without instrument (so educ instead of paeduc)
                     educ + year + age + as.factor(relig) + as.factor(region) + as.factor(dwelown) 
  # then a vertical bar followed by thesame equation again but this time with
  # the instrument (so paeduc instead of educ)
                   | paeduc + year + age + as.factor(relig) + as.factor(region) + as.factor(dwelown),
                   data = sub)
summary(iv.attend)

# extract estimates and standard errors (without the intercept)
iv.coef <- coef(summary(iv.attend))[-1,1:2] 

  # plot coefficients from ols and iv models
    # install.packages("arm")
library(arm)
?coefplot
coefplot(iv.coef[,1], iv.coef[,2], offset = .2, intercept = F,
         varnames = c("educ","year","age", substring(row.names(iv.coef)[-c(1:3)],10), var.las = 2, vertical = F))
coefplot(lm.attend, add = TRUE, col = "red")

# Avoiding weak instruments
  # add diagnostics = T to summary command to get results of several diagnostic
  # tests including:
    # F test of the first stage regression for weak instruments
    # Wu-Hausman test for endogeneity
summary(iv.attend, diagnostics = T) 
  
  # or if we only want to see the "Weak instruments" test we can extract it
summary(iv.attend, diagnostics = T)$diagnostics["Weak instruments",]


# IV increases our standard errors
  # compare confidence intervals
confint(lm.attend)["educ",]
confint(iv.attend)["educ",]

# Check if IV (paeduc) is associated with outcome (n.attend)
summary(lm(n.attend ~ paeduc + year + age + as.factor(relig) 
           + as.factor(region) + as.factor(dwelown), data = sub))


# Check for endogeneity
  # look at Wu-Hausman test
summary(iv.attend, diagnostics = T)
summary(iv.attend, diagnostics = T)$diagnostics["Wu-Hausman",]


### Behind the scenes of IV regression, 2SLS ###
  # stage1: x = controls + iv
  # stage2: y = controls + fitted 

vars <- c("attend", "educ", "paeduc", "year", "age", "relig", "region", "dwelown")
sub <- GSS[, vars]
sub <- na.omit(sub)
Tab(sub$attend)
sub$n.attend <- mapvalues(sub$attend,
                          from = 0:8,
                          to = c(0, 0.5, 2, 12, 24, 30, 40, 52, 88))
Tab(sub$n.attend)

region.labels <- c("NewEng", "MidAtl", "E.N.Centr", 
                   "W.N.Centr", "S.Atl", "E.S.Centr",
                   "W.S.Centr", "Mountain", "Pacific")
sub$f.region <- factor(sub$region,
                       labels = region.labels)
Tab(sub$f.region)

sub$f.relig <- factor(sub$relig)
sub$f.dwelown <- factor(sub$dwelown)

# OLS 
ols <- lm(n.attend ~ educ + year + age + f.region + f.relig + f.dwelown, data = sub)

# Stage 1
stage1 <- lm(educ ~ paeduc + year + age + f.region + f.relig + f.dwelown, data = sub)
summary(stage1)
sub$educ.pred <- predict(stage1) # fitted values

# Stage 2
stage2 <- lm(n.attend ~ educ.pred + year + age + f.region + f.relig + f.dwelown, data = sub)
summary(stage2)


encomptest(
  n.attend ~ year + age + f.region + f.relig + f.dwelown + educ.pred,
  n.attend ~ year + age + f.region + f.relig + f.dwelown + educ,
  data = sub)


stage1Ftest<- encomptest(
  educ ~ year + age + f.region + f.relig + f.dwelown, 
  educ ~ paeduc, 
  data = sub)
stage1Ftest


varnames <- c("intercept", "educ", "year", "age", region.labels[-1], 
              paste("relig", 2:13), paste("dwel", 2:3))


coefplot(stage2, col.pts = "blue", pch.pts = 3, offset = .2, varnames = varnames, intercept = FALSE)
coefplot(ols, add = TRUE, col = "red")
