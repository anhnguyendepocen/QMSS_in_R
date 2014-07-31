#======================================# 
#====== PREDICTION & FORECASTING ======# 
#======================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/29/2014




# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 


#load packages
library(QMSS)
library(ggplot2)
library(plyr)


# load data
load("GSS.RData")



# Prediction --------------------------------------------------------------
# _________________________________________________________________________

### The question: how many children under the age of 18 will there be next year? ###


vars <- c("cohort", "attend", "year", "babies", "preteen", "teens")
sub <- GSS[, vars]

# Recodes (using mutate from plyr, but could also use within(sub, ) )
sub$kidslt18 <- with(sub, babies + preteen + teens)

# get means by year
by.year <- aggregate(subset(sub, sel = c(attend, kidslt18)), list(year = sub$year), mean, na.rm = T)

# interpolate for some missing years
by.year[30:40, "year"] <- c(1979, 1981, 1992, 1995, seq(1997, 2009, 2))
by.year <- arrange(by.year, year)
by.year.ts <- ts(by.year)
by.year.ts <- na.approx(by.year.ts)

# ARIMA(2,0,0) 
arima.dat <- cbind(year = by.year.ts[,"year"],
                   kids = by.year.ts[,"kidslt18"], 
                   L1.kids = lag(by.year.ts[,"kidslt18"], k = -1),
                   L2.kids = lag(by.year.ts[,"kidslt18"], k = -2),
                   L1.attend = lag(by.year.ts[,"attend"], k = -1),
                   L2.attend = lag(by.year.ts[,"attend"], k = -2))

xreg <- arima.dat[,c("L1.kids", "L2.kids", "L1.attend", "L2.attend")]
arima.kids <- arima(arima.dat[,"kids"], order = c(2,0,0), xreg = xreg)
arima.kids

# Predict
# install.packages("forecast")
library(forecast)

# one-step in-sample forecasts
f <- forecast(arima.kids, xreg = xreg, level = 95)
arima.dat <- data.frame(arima.dat, f)

# plot the original data, predictions, and prediction intervals
with(arima.dat, {
     plot(year, Point.Forecast, bty = "l", type = "l", col = "blue", 
          ylab = "", main = "One step ahead forecasts")
     lines(year, Lo.95, lty = 2)
     lines(year, Hi.95, lty = 2)
     lines(year, kids, lty = 3, col = "red")
     legend("topright", c("Data", "Forecast", "Forecast Intervals"), 
            lty = c(3,1,2,2), col = c("red", "blue", "black", "black"), bty = "n")
     })

# make equivalent plot in ggplot
g_f <- ggplot(arima.dat, aes(x = year, y = kids, ymin = Lo.95, ymax = Hi.95)) + ylab("") 
  # add the original data 
(g_f <- g_f + geom_line(color = "red", linetype = 3))
  # add the forecasts
(g_f <- g_f + geom_line(aes(y = Point.Forecast), color = "blue"))
  # show the forecast intervals
g_f + geom_line(aes(y = Lo.95), linetype = 2) + geom_line(aes(y = Hi.95), linetype = 2)
  # or show the forecast intervals as error bars (this is why we put ymin and ymax in the original call to ggplot above)
g_f + geom_errorbar(color = "blue4", size = .2)


# or plot in ggplot a different way
library(reshape2)
molten.f <- melt(arima.dat, id.vars = "year", measure.vars = c("Point.Forecast", "Lo.95", "Hi.95", "kids"))
ggplot(molten.f, aes(x = year, y = value, color = variable, linetype = variable)) + geom_line()





# Prediction error --------------------------------------------------------
# _________________________________________________________________________


# forecast error from arima model
arima.error <- ddply(arima.dat, "year", summarise,
                     kids = kids,  # raw data
                     F = Point.Forecast, # forecast from arima model
                     eF = kids - Point.Forecast,  # forecast error from the arima model
                     eF_sq = eF^2)  # squared error from arima model

  # remove last 2 rows, which are just NAs 
nr <- nrow(arima.error)
arima.error <- arima.error[-c(nr-1, nr), ] 
round(arima.error, 4) 

  # MSFE for arima forecasts
MSFE.arima <- mean(arima.error$eF_sq, na.rm = T)
round(MSFE.arima, 4)

# forecast error using naive lagged model
lag.error <- ddply(arima.dat, "year", summarise,                 
                   kids = kids,  # raw data  
                   F = L1.kids, # forecast using just the lagged data
                   eF = kids - L1.kids, # forecast error using naive lagged model
                   eF_sq = eF^2) # squared error using naive lagged model

lag.error <- lag.error[-c(nr-1, nr), ] 
round(lag.error, 4)

  # MSFE for naive lag forecasts
MSFE.lag <- mean(lag.error$eF_sq, na.rm = T)
round(MSFE.lag, 4)

# ratio of MSFEs from arima and lag forecasts
ratio.arima.lag <- MSFE.arima/MSFE.lag
round(ratio.arima.lag, 4)



# forecast error from OLS model
ols <- lm(kids ~ L1.kids + L2.kids + L1.attend + L2.attend, data = arima.dat)
MSFE.ols <- mean(ols$resid^2)
round(MSFE.ols, 4)

# ratio of MSFEs from ols and lag forecasts
ratio.ols.lag <- MSFE.ols/MSFE.lag
round(ratio.ols.lag, 4)


