## to do:
  # figure out unit root test stuff for all examples
    # ?ur.ers
    # ?embed
    # help(package = "fUnitRoots")

#=========================================#
#====== AR(1) EXAMPLE, ARIMA MODELS ======#
#=========================================#

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


# Another worked example, with some AR(1) after trend ---------------------
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
                     by = list(year = sub$year), FUN = mean, na.rm = T)
by.year



# interpolate for some missing years
# add the extra years
by.year[nrow(by.year) + 1:3, "year"] <- c(1979, 1981, 1992)
by.year <- arrange(by.year, year)
# make a time series object by.year.ts & interpolate with na.approx
by.year.ts <- na.approx(ts(by.year))

# calculate pct under 50 married, under 50 with BA
by.year.ts <- mutate(as.data.frame(by.year.ts), 
                     marriedlt50_pct = 100*marriedlt50,
                     degreelt50_pct = degreelt50*100)

# only keep up to 1992
by.year.ts <- ts(subset(by.year.ts, year <= 1992), start = 1972, end = 1992)

# save the time series (we'll use it again in a different R document)
save(by.year.ts, file = "married_degree_TS.RData")


# make plots
keep.vars <- c("year", "marriedlt50_pct", "degreelt50_pct")
plot.dat <- meltMyTS(by.year.ts, time.var = "year", keep.vars = keep.vars)
rotate_xlabs <- theme(axis.text.x = element_text(angle = 90))  

g_Mar <- ggMyTS(plot.dat, "marriedlt50_pct") + ylab("Pct married (under 50 yrs old)")
g_Mar

# there's a function called custom_xlabs in the QMSS package that makes it
# quicker to customize the appearance of the x-axis text / tick labels
?custom_xlabs
g_Mar + custom_xlabs(angle = 65)

g_Deg <- ggMyTS(plot.dat, "degreelt50_pct") + ylab("Pct with BA (under 50 yrs old)")
g_Deg + custom_xlabs(65)

g_MarDeg <- ggMyTS(df = plot.dat, varlist = c("marriedlt50_pct", "degreelt50_pct"))
g_MarDeg + custom_xlabs(65)


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

# include year trend 
lm.married2 <- lm(marriedlt50_pct ~ degreelt50_pct + year, data = by.year.ts)
summary(lm.married2)

# look for autocorrelation in errors
e2 <- lm.married2$resid
acf(e2, col = "red", lwd = 2, ci.type = "ma") 
plot(e2)
dwtest(lm.married2) 
bgtest(lm.married2) 

# Unit Root Tests
  # install.packages("fUnitRoots")
library(fUnitRoots)

  # many many different unit root tests
?UnitrootTests
?UnitrootUrcaInterface

adfTest(by.year.ts[,"marriedlt50_pct"], lags = 0)
unitrootTest(by.year.ts[,"marriedlt50_pct"], lags = 0)
adfTest(by.year.ts[,"marriedlt50_pct"], lags = 1) 
unitrootTest(by.year.ts[,"marriedlt50_pct"], lags = 1)

urdfTest(by.year.ts[,"marriedlt50_pct"], lags = 8)
urersTest(by.year.ts[,"marriedlt50_pct"], lag.max = 8)


  # add trend
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 0, type = "trend"))
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 1, type = "trend"))
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 8, type = "trend"))
  # add drift
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 0, type = "drift"))
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 1, type = "drift"))
summary(ur.df(by.year.ts[,"marriedlt50_pct"], lags = 8, type = "drift"))


# Phillips-Perron test
summary(ur.pp(by.year.ts[,"marriedlt50_pct"], model = "trend"))


# What do we do if we have unit roots: 
  # difference the data
lm.Dmarried <- lm(firstD(marriedlt50_pct) ~ firstD(degreelt50_pct) + year, data = by.year.ts)
summary(lm.Dmarried)
e3 <- lm.Dmarried$resid
acf(e3, col = "red", lwd = 2, ci.type = "ma") 
plot(e3)
dwtest(lm.Dmarried) # Durbin-Watson test
bgtest(lm.Dmarried) # Breusch-Godfrey test
bgtest(lm.Dmarried, order = 4) 




# ARIMA -------------------------------------------------------------------
# _________________________________________________________________________

xvars <- by.year.ts[,c("degreelt50_pct", "year")]

# ARIMA(1,0,0) = AR(1)
arima.married100 <- arima(by.year.ts[,"marriedlt50_pct"], order = c(1,0,0), xreg = xvars)
arima.married100
tsdiag(arima.married100)

# ARIMA(0,1,0) = First differences
arima.married010 <- update(arima.married100, order = c(0,1,0))
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

rotate_xlabs <- theme(axis.text.x = element_text(angle = 90))
varlist.married <- c("marriedlt50_pct", "D1.marriedlt50_pct", "D2.marriedlt50_pct")
varlist.degree <- c("degreelt50_pct", "D1.degreelt50_pct", "D2.degreelt50_pct")
ggMyTS(Diff.dat, varlist = varlist.married) + rotate_xlabs
ggMyTS(Diff.dat, varlist = varlist.degree) + rotate_xlabs


# Box-Pierce (a.k.a portmanteau) test for white noise 
# install.packages("TSA")
library(TSA)
LB.test(arima.married520, type = "Box-Pierce")


# ARIMA(5,1,0) 
arima.married510 <- update(arima.married520, order = c(5,1,0))
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
                      unemployment = unempl)
plot.dat <- melt(plot.dat, id.vars = "year")

year_labs <- scale_x_continuous(breaks = seq(1948,2012,4)) 
axis_and_legend_opts <- theme(axis.text.x = element_text(angle = 60, vjust = 0.5), 
                              legend.position = "bottom") 

gg_fatal <- ggplot(plot.dat, aes(x = year, y = value, group = variable, color = variable)) 
(gg_fatal + geom_line(size = 1) + stat_smooth(method = "lm", se = F, lty = 2) 
 + year_labs + axis_and_legend_opts)


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
