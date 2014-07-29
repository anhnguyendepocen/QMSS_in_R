## to do:
  # figure out unit root test stuff for all example
    # ?ur.ers
    # ?embed
    # help(package = "fUnitRoots")

#=========================================#
#====== AR(1) EXAMPLE, ARIMA MODELS ======#
#=========================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/25/2014



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




# Another time series problem ---------------------------------------------
# _________________________________________________________________________

# For 1975-1992: does an increasing percentage of people under 50 with BAs lead
# to a decreasing percentage of people under 50 being married?

vars <- c("cohort", "year", "sex", "age", "marital", "degree")
sub <- GSS[, vars]

# Recodes (using mutate from plyr, but could also use within(sub, ) )
sub <- mutate(sub, 
              married = ifelse(marital == 1, 1, 0),
              baplus = ifelse(degree >= 3, 1, 0),
              marriedlt50 = ifelse(married == 1 & age < 50, 1, 0),
              degreelt50 = ifelse(baplus == 1 & age <50, 1, 0))


# get means by year
by.year <- aggregate(subset(sub, sel = c(marriedlt50, degreelt50)), 
                     list(year = sub$year), mean, na.rm = T)
by.year

# interpolate for some missing years
# add the extra years
by.year[30:32, "year"] <- c(1979, 1981, 1992)
by.year <- arrange(by.year, year)
# make a time series object by.year.ts & interpolate with na.approx
by.year.ts <- ts(by.year)
by.year.ts <- na.approx(by.year.ts)

# calculate pct under 50 married, under 50 with BA
by.year.ts <- data.frame(by.year.ts)
by.year.ts <- mutate(by.year.ts, 
                     marriedlt50_pct = 100*marriedlt50,
                     degreelt50_pct = degreelt50*100)

# only keep up to 1992
by.year.ts <- ts(subset(by.year.ts, year <= 1992), start = 1972, end = 1992)

# save the time series (we'll use it again in a different R document)
save(by.year.ts, file = "married_degree_TS.RData")

# make plots
keep.vars <- c("year", "marriedlt50_pct", "degreelt50_pct")
plot.dat <- meltMyTS(by.year.ts, time.var = "year", keep.vars = keep.vars)

g_Mar <- ggMyTS(plot.dat, "marriedlt50_pct") + ylab("Pct married (under 50 yrs old)")
g_Mar + theme(axis.text.x = element_text(angle = 90))  
g_Deg <- ggMyTS(plot.dat, "degreelt50_pct") + ylab("Pct with BA (under 50 yrs old)")
g_Deg + theme(axis.text.x = element_text(angle = 90))  

g_MarDeg <- ggMyTS(df = plot.dat, varlist = c("marriedlt50_pct", "degreelt50_pct"))
g_MarDeg + theme(axis.text.x = element_text(angle = 90))  


# correlations
cor.vars <- c("marriedlt50_pct", "degreelt50_pct", "year")
cor(by.year.ts[,cor.vars], use = "complete")

# simplest regression
lm.married <- lm(marriedlt50_pct ~ degreelt50_pct, data = by.year.ts)
summary(lm.married)

# look for autocorrelation in errors
e <- lm.married$resid
acf(e, col = "red", lwd = 2, ci.type = "ma") 
dwtest(lm.married) # Durbin-Watson test
bgtest(lm.married) # Breusch-Godfrey test

# include year trend and use robust std errors 
lm.married2 <- lm(marriedlt50_pct ~ degreelt50_pct + year, data = by.year.ts)
summary(lm.married2)

# look for autocorrelation in errors
e2 <- lm.married2$resid
acf(e2, col = "red", lwd = 2, ci.type = "ma") 
plot(e2)
dwtest(lm.married2) 
bgtest(lm.married2) 

# Dickey-Fuller Unit Root Test

# install.packages("fUnitRoots")
library(fUnitRoots)
urersTest(by.year.ts[,"marriedlt50_pct"], lag.max = 0)
adfTest(by.year.ts[,"marriedlt50_pct"], lags = 0)
adfTest(by.year.ts[,"marriedlt50_pct"], lags = 1)  

library(urca)
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 0))
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 1))
urdfTest(by.year.ts[,"marriedlt50_pct"], lags = 8)

  # add trend
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 0, type = "trend"))
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 8, type = "trend"))
  # add drift
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 0, type = "drift"))
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 8, type = "drift"))


# Phillips-Perron test
PP.test(by.year.ts[,"marriedlt50_pct"])
summary(ur.pp(by.year.ts[,"marriedlt50_pct"], model = "trend"))


# What do we do if we have unit roots: 
  # difference the data
lm.Dmarried <- lm(firstD(marriedlt50_pct) ~ firstD(D.degreelt50_pct) + year, data = by.year.ts)
summary(lm.Dmarried)
e3 <- lm.Dmarried$resid
acf(e3, col = "red", lwd = 2, ci.type = "ma") 
plot(e3)
dwtest(lm.Dmarried) # Durbin-Watson test
bgtest(lm.Dmarried) # Breusch-Godfrey test
bgtest(lm.Dmarried, order = 4) 




# ARIMA -------------------------------------------------------------------
# _________________________________________________________________________

# ARIMA(1,0,0) = AR(1)
arima.married100 <- arima(by.year.ts[,"marriedlt50_pct"], 
                       order = c(1,0,0), 
                       xreg = by.year.ts[,c("degreelt50_pct", "year")])
arima.married100
tsdiag(arima.married100)

# ARIMA(0,1,0) = First differences
arima.married010 <- arima(by.year.ts[,"marriedlt50_pct"], 
                          order = c(0,1,0), 
                          xreg = by.year.ts[,c("degreelt50_pct","year")])
arima.married010
tsdiag(arima.married010)


# ARIMA(5,2,0) = WTF?
arima.married520 <- arima(by.year.ts[,"marriedlt50_pct"], 
                          order = c(5,2,0), 
                          xreg = by.year.ts[,"degreelt50_pct"])
arima.married520
tsdiag(arima.married520)

# What do the first and second differenced variables looks like?
Diff.dat <- cbind(year = by.year.ts[,"year"], 
                  marriedlt50_pct = by.year.ts[,"marriedlt50_pct"], 
                  D1.marriedlt50_pct = diff(by.year.ts[,"marriedlt50_pct"], diff = 1), 
                  D2.marriedlt50_pct = diff(by.year.ts[,"marriedlt50_pct"], diff = 2),
                  degreelt50_pct = by.year.ts[,"degreelt50_pct"], 
                  D1.degreelt50_pct = diff(by.year.ts[,"degreelt50_pct"], diff = 1), 
                  D2.degreelt50_pct = diff(by.year.ts[,"degreelt50_pct"], diff = 2))
Diff.dat <- meltMyTS(Diff.dat, "year")

(ggMyTS(Diff.dat, varlist = c("marriedlt50_pct", "D1.marriedlt50_pct", "D2.marriedlt50_pct"))
 + theme(axis.text.x = element_text(angle = 90)))

(ggMyTS(Diff.dat, varlist = c("degreelt50_pct", "D1.degreelt50_pct", "D2.degreelt50_pct"))
 + theme(axis.text.x = element_text(angle = 90)))

# Box-Pierce (a.k.a portmanteau) test for white noise 
# install.packages("TSA")
library(TSA)
LB.test(arima.married520, type = "Box-Pierce")


# ARIMA(5,1,0) = WTF?
arima.married510 <- arima(by.year.ts[,"marriedlt50_pct"], 
                          order = c(5,1,0), 
                          xreg = by.year.ts[,"degreelt50_pct"])
arima.married510
tsdiag(arima.married510)
LB.test(arima.married510, type = "Box-Pierce")



# Unemployment & fatalities example ---------------------------------------
# _________________________________________________________________________

# Does an increase in the unemployment rate really relate to the rate of traffic
# fatalities (based on vehicle miles)?

library(foreign)
fatal.unemp <- read.dta("fatalities-unemployment.dta")
fatal.unemp <- rename(fatal.unemp, replace = c("umempl" = "unempl"))
plot.dat <- summarise(fatal.unemp,
                      year = year,
                      fatalities = fatpbvmt,
                      fatalities_fitted = lm(fatalities ~ year)$fitted,
                      unemployment = unempl,
                      unemployment_fitted = lm(unemployment ~ year)$fitted)
plot.dat <- meltMyTS(plot.dat, time.var = "year")
gg_fatal <- ggMyTS(plot.dat, point = F) 
gg_fatal + theme(axis.text.x = element_text(angle = 90)) + scale_x_continuous(breaks = seq(1948,2012,4))


# First model
lm.fatal <- lm(fatpbvmt ~ unempl, data = fatal.unemp)
error <- lm.fatal$resid

# Get partial autocorrelations
pacf(error, plot = F)

# Include trend
lm.fatal2 <- lm(fatpbvmt ~ unempl + year, data = fatal.unemp)
summary(lm.fatal2)
error2 <- lm.fatal2$resid
print(pacf(error2))

# Use first differences model
lm.Dfatal <- lm(firstD(fatpbvmt) ~ firstD(unempl) + year, data = fatal.unemp)
summary(lm.Dfatal)
errorD <- lm.Dfatal$resid
durbinWatsonTest(lm.Dfatal, max.lag = 4)
