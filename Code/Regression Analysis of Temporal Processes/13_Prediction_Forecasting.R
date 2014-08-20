#======================================# 
#====== PREDICTION & FORECASTING ======# 
#======================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3




# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory 
setwd("INSERT PATH TO DIRECTORY") 


#load packages
library(QMSS)
library(ggplot2)
library(plyr)


# load data
load("GSS.RData")



# One step ahead forecasts ------------------------------------------------
# _________________________________________________________________________

### The question: how many children under the age of 18 will there be next year? ###

vars <- c("cohort", "attend", "year", "babies", "preteen", "teens")
sub <- subset(GSS, year <= 2010, select = vars)

# Recodes (using mutate from plyr, but could also use within(sub, ) )
sub$kidslt18 <- with(sub, babies + preteen + teens)

# get means by year
by.year <- aggregate(subset(sub, sel = c(attend, kidslt18)), 
                     by = list(year = sub$year), FUN = mean, na.rm = T)

# interpolate for some missing years
missing.years <- c(1979, 1981, 1992, 1995, seq(1997, 2009, 2))
nr <- nrow(by.year)
new.rows <- (nr + 1):(nr + length(missing.years))
by.year[new.rows, "year"] <- missing.years
by.year <- arrange(by.year, year)
by.year.ts <- na.approx(ts(by.year))
by.year.ts <- ts(by.year.ts, start = 1972, end = 2010)


# ARIMA(2,0,0) 
arima.dat <- cbind(year = by.year.ts[,"year"],
                   kids = by.year.ts[,"kidslt18"], 
                   L1.kids = lag(by.year.ts[,"kidslt18"], k = -1),
                   L2.kids = lag(by.year.ts[,"kidslt18"], k = -2),
                   L1.attend = lag(by.year.ts[,"attend"], k = -1),
                   L2.attend = lag(by.year.ts[,"attend"], k = -2))

nr <- nrow(arima.dat)
arima.dat <- arima.dat[-c(nr, nr-1), ]

xvars <- arima.dat[,c("L1.kids", "L2.kids", "L1.attend", "L2.attend")]
arima.kids <- arima(arima.dat[,"kids"], order = c(2,0,0), xreg = xvars)
arima.kids


# Predict
# install.packages("forecast")
library(forecast)

# one-step in-sample forecast ahead 
Point.Forecast <- fitted(arima.kids)
se <- sqrt(arima.kids$sigma2)
Lo.95 <- Point.Forecast - 1.96*se
Hi.95 <- Point.Forecast + 1.96*se
arima.preds <- data.frame(arima.dat, Point.Forecast, Lo.95, Hi.95)



# plot the original data, predictions, and prediction intervals
with(arima.preds, {
  plot(year, kids, bty = "l", type = "l", col = "red", lty = 3, lwd = 2,
       ylab = "", main = "One step ahead forecasts")
  lines(year, Lo.95, lty = 2)
  lines(year, Hi.95, lty = 2)
  lines(year, Point.Forecast, col = "blue")
  legend("topright", c("Data", "Forecast", "Forecast Intervals"), 
         lty = c(3,1,2,2), col = c("red", "blue", "black", "black"), bty = "n")
})


# with ggplot
g_f <- ggplot(arima.preds, aes(x = year, y = kids, ymin = Lo.95, ymax = Hi.95)) 
# add the original data 
(g_f <- g_f + geom_line(color = "red", linetype = 3, size = 1.25))
# add the forecasts
(g_f <- g_f + geom_line(aes(y = Point.Forecast), color = "blue"))
# show the forecast intervals
g_f + geom_line(aes(y = Lo.95), linetype = 2) + geom_line(aes(y = Hi.95), linetype = 2)
# or show the forecast intervals as error bars or point ranges (this is why we put ymin and ymax in the original call to ggplot above)
g_f + geom_errorbar(color = "blue4", size = .2)
g_f + geom_pointrange(color = "blue4", size = .2)





# Forecast error ----------------------------------------------------------
# _________________________________________________________________________

# Arima model: table of year, kids, forecast, forecast error, forecast error squared
arima.error <- ddply(arima.preds, "year", summarise,
                     kids = kids,  # raw data
                     F = Point.Forecast, # forecast from arima model
                     eF = kids - F,  # forecast error from the arima model
                     eF_sq = eF^2)  # squared error from arima model
round(arima.error, 4) 

# MSFE for arima forecasts
MSFE.arima <- mean(arima.error$eF_sq, na.rm = T)
round(MSFE.arima, 4)

# same table for naive lagged model
lag.error <- ddply(arima.preds, "year", summarise,                 
                   kids = kids,  # raw data  
                   F = L1.kids, # forecast using just the lagged data
                   eF = kids - F, # forecast error using naive lagged model
                   eF_sq = eF^2) # squared error using naive lagged model
round(lag.error, 4)

# MSFE for naive lag forecasts
MSFE.lag <- mean(lag.error$eF_sq, na.rm = T)
round(MSFE.lag, 4)

# ratio of MSFEs from arima and lag forecasts
ratio.arima.lag <- MSFE.arima/MSFE.lag
round(ratio.arima.lag, 4)


# MSFE for OLS model
ols <- lm(kids ~ L1.kids + L2.kids + L1.attend + L2.attend, data = arima.dat)
MSFE.ols <- mean(ols$resid^2)
round(MSFE.ols, 4)

# ratio of MSFEs from ols and lag forecasts
ratio.ols.lag <- MSFE.ols/MSFE.lag
round(ratio.ols.lag, 4)




# Testing on holdout data -------------------------------------------------
# _________________________________________________________________________

y.training <- ts(arima.dat[1:28, "kids"], start = 1972, end = 1999)
x.training <- arima.dat[1:28, 3:6]
x.testing <- arima.dat[29:39, 3:6]

arima.training <- auto.arima(y.training, xreg = x.training)
multistep <- forecast(arima.training, xreg = x.testing)

plot(multistep, lwd = 0, fcol = "red3", flty = 2, 
     shadecols = c("skyblue4", "gray"), main = "Multi-step ahead forecasts")
lines(fitted(arima.training), col = "forestgreen", lwd = 2)
lines(ts(arima.dat[,"kids"], start = 1972, end = 2010), lwd = 2)
legend("topright", c("Data", "One-step", "Multi-step with prediction intervals"), 
       col = c("black", "forestgreen", "red3"),
       lty = c(1,1,2), lwd = c(2,1,2), bty = "n")

