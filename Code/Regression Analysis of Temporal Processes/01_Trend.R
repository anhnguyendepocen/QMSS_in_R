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

vars <- c("natcrime", "year", "sex")
sub <- GSS[, vars]
sub$n.natcrime <- ReverseThis(sub$natcrime)
Tab(sub$n.natcrime)
sub <- na.omit(sub)

# Graph trend over time
  # get mean of n.natcrime by year
by.year <- ddply(sub, "year", summarise, 
                 mean = mean(n.natcrime, na.rm = T))
  # plot the trend
plot(by.year, type = "l", lwd = 2, col = "navyblue", bty = "l")

  # add a fitted line 
with(by.year, abline(line(year, mean), col = "maroon", lwd = 2, lty = 2))

  # Or with ggplot
g_by.year <- ggplot(by.year, aes(x = year, y = mean)) + geom_line(color = "navyblue")
g_by.year

  # add a fitted line
g_by.year + stat_smooth(method = "lm", se = FALSE, color = "maroon")



# Graph trend over time by gender
  # get mean of n.natcrime by year and gender
by.year.sex <- ddply(sub, c("year", "sex"), summarise, 
                 mean = mean(n.natcrime, na.rm = T))

  # plot the trend
with(by.year.sex,{
  plot(year, mean, type = "n", bty = "l")
  lines(year[sex == 1], mean[sex == 1], lwd = 2, col = "navyblue")
  lines(year[sex == 2], mean[sex == 2], lwd = 2, col = "darkred")
})

  # add fitted lines
with(by.year.sex, {
     abline(line(year[sex == 1], mean[sex == 1]), col = "skyblue", lwd = 2, lty = 2)
     abline(line(year[sex == 2], mean[sex == 2]), col = "maroon", lwd = 2, lty = 2)
})

  # add a legend
legend("bottomleft", c("Male","Female"),
       col = c("navyblue", "darkred"), lwd = 2, bty = "n")

# Or with ggplot
g_by.year.sex <- ggplot(by.year.sex, aes(x=year, y=mean, group=sex)) + geom_line(aes(color=factor(sex))) 
g_by.year.sex 
g_by.year.sex + stat_smooth(method = "lm", se = FALSE, lty = 2, aes(color = factor(sex)))


# Quadratic fit
x <- with(by.year.sex, seq(min(year),max(year),len=200))
y1 <- lm(mean~year+I(year^2), data = by.year.sex, sex == 1)$coef %*% rbind(1,x,x^2)
y2 <- lm(mean~year+I(year^2), data = by.year.sex, sex == 2)$coef %*% rbind(1,x,x^2)
plot(NULL, xlim = c(min(x), max(x)), ylim = c(min(y1,y2), max(y1,y2)),
     xlab = "Year", ylab = "", bty = "l")
lines(x,y1,lwd=2,col="cyan3")
lines(x,y2,lwd=2,col="hotpink")
legend("bottomleft", c("Male", "Female"), lwd = 2, col = c("cyan3", "hotpink"))

# Cubic fit
x <- with(by.year.sex, seq(min(year),max(year),len=200))
y1 <- lm(mean~year+I(year^2) + I(year^3), data = by.year.sex, sex == 1)$coef %*% rbind(1,x,x^2,x^3)
y2 <- lm(mean~year+I(year^2) + I(year^3), data = by.year.sex, sex == 2)$coef %*% rbind(1,x,x^2,x^3)
plot(NULL, xlim = c(min(x), max(x)), ylim = c(min(y1,y2), max(y1,y2)),
     xlab = "Year", ylab = "", bty = "l")
lines(x,y1,lwd=2,col="cyan3")
lines(x,y2,lwd=2,col="hotpink")
legend("bottomleft", c("Male", "Female"), lwd = 2, col = c("cyan3", "hotpink"))




# Naive OLS ---------------------------------------------------------------
# _________________________________________________________________________

lm.natcrime <- lm(n.natcrime ~ year, data = sub)
summary(lm.natcrime)

# Test for heteroskedasticity 
# using bptest() from lmtest package
bptest(lm.natcrime) 

# Robust standard errors
# using robcov() and ols() from rms package to get Huber-White estimator of covariance matrix
# the values reported in the S.E. column are now robust standard errors
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

# plot mean and sd of residuals by year
with(resids.by.year, {      
  # first make a plot window with right dimensions & labels
  sub.txt <- paste("Correlation =", round(cor.resids, 3))
  plot(NULL, bty = "l", xlab = "", ylab = "", 
       ylim = c(min(sd,mean), max(sd,mean)), 
       xlim = range(year),
       sub = sub.txt) 
  # add the trends 
  lines(year, sd, lwd = 2, col = "purple4")
  lines(year, mean, lwd = 2, col = "orangered")
  # label the lines 
  text(x = mean(year), y = min(sd), pos = 1, labels = "Std. Dev. of the error")
  text(x = mean(year), y = max(mean), pos = 3, labels = "Mean of the error")
})



# regression with dummy variable for each year (and robust std. errors)
ols.natcrime2 <- robcov(ols(n.natcrime ~ as.factor(year), data = sub, x = T, y = T))
ols.natcrime2



# Interactions ------------------------------------------------------------
# _________________________________________________________________________

# ols with clustered SEs for males
ols.natcrimeM <- robcov(ols(n.natcrime ~ year, data = sub, sex == 1, x = T, y = T), cluster = sub$year)

# ols with clustered SEs for females
ols.natcrimeF <- robcov(ols(n.natcrime ~ year, data = sub, sex == 2, x = T, y = T), cluster = sub$year)

# test for equality/difference of coefficients
  # we can just check if coeff on interaction term is significantly different from 0
robcov(ols(n.natcrime ~ year*factor(sex), data = sub, x = T, y = T), cluster = sub$year)

### Has men’s concern over crime dropped faster over the last 40 year than women’s? ###
sub$male <- sub$sex==1

# without interaction
ols.natcrime3 <- robcov(ols(n.natcrime ~ year + male, data = sub, x = T, y = T), cluster = sub$year)
ols.natcrime3

# with interacton
ols.natcrime4 <- robcov(ols(n.natcrime ~ year*male, data = sub, x = T, y = T), cluster = sub$year)
ols.natcrime4

# get predicted values for all combinations of year & male in the data
pred.dat <- expand.grid(year = unique(sub$year), male = c(TRUE,FALSE))
pred.dat <- data.frame(pred.dat, p =  predict(ols.natcrime4, pred.dat))

# plot lines for males and females separately
with(pred.dat,{
  plot(year, p, type = "n", bty = "l")
  lines(year[male == T], p[male == T], lwd = 3, col = "navyblue")
  lines(year[male == F], p[male == F], lwd = 3, col = "darkred")
  legend("bottomleft", c("Male", "Female"), lwd = 2, col = c("navyblue", "darkred"), bty = "n")
})

# with ggplot
g_by.sex <- ggplot(data = pred.dat, aes(x=year, y=p, color=male)) + stat_smooth(method = lm, lwd = 1.5)
g_by.sex 


# Logit -------------------------------------------------------------------
# _________________________________________________________________________

# indicator for n.natcrime = 3
sub$muchconcern <- sub$n.natcrime == 3
# make year variable relative to the first year (this helps avoid an error below due to a singular design matrix)
sub$n.year <- with(sub, year - min(year))

# fit logit models for men and women with lrm() so we can use robcov to get clustered SEs
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
logit.predsM <- cbind(year = unique(sub$n.year) + 1973, 
                      predict(lrm.muchconcernM, type = "lp", newdata = unique(sub$n.year)))
logit.predsF <- cbind(year = unique(sub$n.year) + 1973, 
                      predict(lrm.muchconcernF, type = "lp", newdata = unique(sub$n.year)))      
plot(logit.predsM, type = "l", lwd = 3, col = "turquoise4", xlab = "year", ylim = c(0, 1))
lines(logit.predsF, col = "purple4", lwd = 3)
legend("bottom", c("Males", "Females"), lwd = 2, col = c("turquoise4", "purple4"), bty = "n")



# Ordinal logit -----------------------------------------------------------
# _________________________________________________________________________

# fit ordinal logit models for men and women with lrm() so we can use robcov to get clustered SEs
lrm.muchconcernM2 <- lrm(n.natcrime ~ n.year, data = sub, subset = male, x = T, y = T)
lrm.muchconcernM2 <- robcov(lrm.muchconcernM2, cluster = with(sub, n.year[male]))
lrm.muchconcernM2

lrm.muchconcernF2 <- lrm(n.natcrime ~ n.year, data = sub, subset = !male, x = T, y = T)
lrm.muchconcernF2 <- robcov(lrm.muchconcernF2, cluster = with(sub, n.year[!male]))
lrm.muchconcernF2

# odds ratios
exp(coef(lrm.muchconcernM2)["n.year"])
exp(coef(lrm.muchconcernF2)["n.year"])

