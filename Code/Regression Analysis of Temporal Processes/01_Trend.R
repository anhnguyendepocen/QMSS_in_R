#====================#
#====== TRENDS ======#
#====================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/20/2014


# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# Set working directory
setwd("/Users/jgabry/Desktop/COLUMBIA/Stuff_for_Greg/Data")

# load packages
library(QMSS)
library(ggplot2)
library(plyr)
library(rms)

# Load the cumulative GSS dataset 
load("GSS.RData")




# Rolling (pooled) cross-sectional data -----------------------------------
# _________________________________________________________________________

# make a subset of the GSS 
vars <- c("natcrime", "year", "sex")
sub <- GSS[, vars]

# reverse-code natcrime using ReverseThis function in QMSS package
sub$n.natcrime <- ReverseThis(sub$natcrime)

# get a table of counts, percentages, and cumulative percentages using Tab from
# QMSS package
Tab(sub$n.natcrime)

# remove missing values from the subset
sub <- na.omit(sub)

# Graph trend over time
  # first get mean of n.natcrime by year using ddply from plyr package
by.year <- ddply(sub, # data frame to use
                 "year", # variable
                 summarise, # function to use 
                 mean = mean(n.natcrime)) # create new variable "mean" 
  # plot the trend
plot(by.year, type = "l", lwd = 2, col = "navyblue", bty = "l")

  # add a fitted line 
with(by.year, abline(line(year, mean), col = "maroon", lwd = 2, lty = 2))

  # Or with ggplot
g_by.year <- ggplot(by.year, aes(x = year, y = mean)) + geom_line(color = "navyblue")
g_by.year

  # add a fitted line
g_by.year + stat_smooth(method = "lm", se = FALSE, color = "maroon", lty = 2)



# Graph trend over time by gender
  # get mean of n.natcrime by year and gender
by.year.sex <- ddply(sub, c("year", "sex"), summarise, 
                 mean = mean(n.natcrime, na.rm = T))

  # define some labels and colors to use
color_and_labels <- scale_color_manual(values = c("navyblue", "darkred"), labels = c("Male","Female"), name = "")
  # set up the plot (declare x, y, grouping and coloring by sex)
g_by.year.sex <- ggplot(by.year.sex, aes(x=year, y=mean, group=sex, color = factor(sex))) 

  # view the trend 
g_by.year.sex <- g_by.year.sex + geom_line() 
g_by.year.sex + color_and_labels
  
  # add fitted lines
g_linear <- g_by.year.sex + stat_smooth(method = "lm", se = FALSE, lty = 2)
g_linear + color_and_labels

  # use a quadratic fit 
g_quad <- g_by.year.sex + stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = F, lty = 2)
g_quad + color_and_labels 

  # use a cubic fit 
g_cubic <- g_by.year.sex + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = F, lty = 2)
g_cubic + color_and_labels





# Naive OLS ---------------------------------------------------------------
# _________________________________________________________________________

lm.natcrime <- lm(n.natcrime ~ year, data = sub)
summary(lm.natcrime)

# Test for heteroskedasticity 
# using bptest() from lmtest package
bptest(lm.natcrime) 

# Robust standard errors
# using robcov() and ols() from rms package to get Huber-White estimator of
# covariance matrix. the values reported in the S.E. column are now robust
# standard errors
ols.natcrime <- ols(n.natcrime ~ year, data = sub, x = T, y = T) # need arguments x=T and y=T (so that robcov() function has access to expanded design matrix)
robcov(ols.natcrime) 

# Clustered standard errors
robcov(ols.natcrime, cluster = sub$year)



# get mean(residual) & sd(residual) by year
sub$resids <- lm.natcrime$residuals
resids.by.year <- ddply(sub, "year", summarise, mean = mean(resids), sd = sd(resids))

# get correlation between mean(residual) & sd(residual)
cor.resids <- with(resids.by.year, cor(mean, sd))
cor.resids

# There are several ways we can plot the mean and sd of the residuals by year
# using ggplot. First we can do it the long way, which takes more code but seems
# like the natural way to do it at first (i.e. it follows a similar logic to how
# you would plot it using R's base graphics instead of ggplot).

  # setup a ggplot using the resids.by.year data with the x-axis corresponding
  # to year
g_resids.by.year <- ggplot(resids.by.year, aes(x = year))
  # add the line for the means
g_resids.by.year <- g_resids.by.year + geom_line(aes(y = mean), color = "purple4") 
  # add the line for the sds
g_resids.by.year <- g_resids.by.year + geom_line(aes(y = sd), color = "orangered")
  # view the plot
g_resids.by.year 
  # remove the label "mean" from the y-axis since we're also plotting sds
g_resids.by.year + ylab("")

# It's surprisingly hard to add a legend to a plot like this in ggplot because 
# the way our data resids.by.year is structured there's no grouping variable 
# (like sex). But we can use a better strategy that makes it easier to make the
# plot and allows for much more flexibility with the legends, layout, etc:

# First we use the melt function in the reshape2 package

# install.packages("reshape2")
library(reshape2)

# We specify id.vars = "year" so that we're still grouping by year. The 
# resulting object is a data frame with one column for year, one column called 
# "variable" and one column called "value". The value column now has the values 
# from both the "means" column and "sd" column of the original resids.by.year 
# data. The "variable" column indicates if the value corresponds to a "mean" or 
# an "sd".
resids.by.year_melt <- melt(resids.by.year, id.vars = "year")
resids.by.year_melt

# Now making the plot with ggplot is much much easier because we can specify 
# "y = value", "group = variable", and "color = variable"
g_resids.by.year2 <- ggplot(resids.by.year_melt, 
                            aes(x = year, y = value, group = variable, color = variable))

# and now we just add geom_line() and we're done
g_resids.by.year2 + geom_line()

# if we want to customize the colors we can also do that like this
g_resids.by.year2 + geom_line() + scale_color_manual(values = c("purple4", "orangered"))



# regression with dummy variable for each year (and robust std. errors)
ols.natcrime2 <- robcov(ols(n.natcrime ~ as.factor(year), data = sub, x = T, y = T))
ols.natcrime2



# Interactions ------------------------------------------------------------
# _________________________________________________________________________

# ols with clustered SEs for males
ols.natcrimeM <- robcov(ols(n.natcrime ~ year, data = sub, sex == 1, x = T, y = T), 
                        cluster = sub$year)

# ols with clustered SEs for females
ols.natcrimeF <- robcov(ols(n.natcrime ~ year, data = sub, sex == 2, x = T, y = T), 
                        cluster = sub$year)

# test for equality/difference of coefficients
  # we can just check if coeff on interaction term is significantly different from 0
robcov(ols(n.natcrime ~ year*factor(sex), data = sub, x = T, y = T), 
       cluster = sub$year)

### Has men’s concern over crime dropped faster over the last 40 year than women’s? ###
sub$male <- sub$sex==1

# without interaction
ols.natcrime3 <- robcov(ols(n.natcrime ~ year + male, data = sub, x = T, y = T), 
                        cluster = sub$year)
ols.natcrime3

# with interacton
ols.natcrime4 <- robcov(ols(n.natcrime ~ year*male, data = sub, x = T, y = T), 
                        cluster = sub$year)
ols.natcrime4

# get predicted values for all combinations of year & male in the data
pred.dat <- expand.grid(year = unique(sub$year), male = c(TRUE,FALSE))
pred.dat <- data.frame(pred.dat, p =  predict(ols.natcrime4, pred.dat))

# plot lines for males and females separately
g_by.sex <- ggplot(data = pred.dat, aes(x=year, y=p, color=male)) 
g_by.sex + stat_smooth(method = lm, lwd = 1.5) + ylab("predicted value")


# Logit -------------------------------------------------------------------
# _________________________________________________________________________

# indicator for n.natcrime = 3
sub$muchconcern <- sub$n.natcrime == 3
# make year variable relative to the first year (this helps avoid an error below
# due to a singular design matrix)
sub$n.year <- with(sub, year - min(year))

# fit logit models for men and women with lrm() so we can use robcov to get
# clustered SEs
lrm.muchconcernM <- lrm(muchconcern ~ n.year, data = sub, subset = male, x = T, y = T)
lrm.muchconcernM <- robcov(lrm.muchconcernM, cluster = with(sub, n.year[male]))
lrm.muchconcernM

lrm.muchconcernF <- lrm(muchconcern ~ n.year, data = sub, subset = !male, x = T, y = T)
lrm.muchconcernF <- robcov(lrm.muchconcernF, cluster = with(sub, n.year[!male]))
lrm.muchconcernF

# chi^2 test for equality of logit coefficicents
bM <- coef(lrm.muchconcernM)["n.year"]
bF <- coef(lrm.muchconcernF)["n.year"]
vM <- vcov(lrm.muchconcernM)["n.year","n.year"]
vF <- vcov(lrm.muchconcernF)["n.year","n.year"]
chi.sq <- (bM - bF)^2/(vM + vF)
pchisq(chi.sq, df = 1, lower.tail = FALSE)


# graph
logit.preds <- data.frame(year = sub$n.year, 
                          Male = predict(lrm.muchconcernM, type = "lp", newdata = sub$n.year),
                          Female = predict(lrm.muchconcernF, type = "lp", newdata = sub$n.year))
logit.preds <- melt(logit.preds, id.vars = "year")

gg_logit.preds <- ggplot(logit.preds, 
                         aes(x = year + 1973, y = value, group = variable, color = variable))
gg_logit.preds + geom_line() + xlab("year")


# Ordinal logit -----------------------------------------------------------
# _________________________________________________________________________

# fit ordinal logit models for men and women with lrm() so we can use robcov to
# get clustered SEs
lrm.muchconcernM2 <- lrm(n.natcrime ~ n.year, data = sub, subset = male, x = T, y = T)
lrm.muchconcernM2 <- robcov(lrm.muchconcernM2, cluster = with(sub, n.year[male]))
lrm.muchconcernM2

lrm.muchconcernF2 <- lrm(n.natcrime ~ n.year, data = sub, subset = !male, x = T, y = T)
lrm.muchconcernF2 <- robcov(lrm.muchconcernF2, cluster = with(sub, n.year[!male]))
lrm.muchconcernF2

# odds ratios
exp(coef(lrm.muchconcernM2)["n.year"])
exp(coef(lrm.muchconcernF2)["n.year"])

