#=========================#
#====== TIME SERIES ======#
#=========================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 


#load packages
library(QMSS)
library(plyr)
library(car)

# load data
load("GSS_panel.RData")
load("GSS.RData")




# A simple time series problem --------------------------------------------
# _________________________________________________________________________

vars <- c("cohort", "confinan", "attend", "pray", "conclerg", "homosex", "year", 
          "sex", "age", "realinc", "marital", "divlaw", "partyid", "polviews", 
          "babies", "preteen", "teens", "wrkstat", "divorce", "degree")

sub <- GSS[, vars]

# Recodes (using mutate from plyr, but could also use within(sub, ) )
?mutate
sub <- mutate(sub, 
              kidslt18 = babies + preteen + teens,
              married = ifelse(marital == 1, 1, 0),
              baplus = ifelse(degree >= 3, 1, 0),
              fulltime = ifelse(wrkstat == 1, 1, 0),
              womenft = ifelse(sex == 2 & wrkstat == 1, 1, 0),
              marriedlt50 = ifelse(married == 1 & age < 50, 1, 0),
              degreelt50 = ifelse(baplus == 1 & age <50, 1, 0),
              n.confinan = ReverseThis(confinan),
              f.partyid = factor(partyid),
              f.divlaw = factor(divlaw),
              divlaw1 = ifelse(divlaw == 1, 1, 0),
              partyid6 = ifelse(partyid == 6, 1, 0),
              partyid7 = ifelse(partyid == 7, 1, 0))


# get means by year
vars2 <- c("conclerg", "n.confinan", "fulltime", "attend", "pray", "degreelt50", "cohort", 
           "homosex", "marriedlt50", "sex", "kidslt18", "age", "realinc", "married", "divlaw", 
           "polviews", "babies", "preteen", "teens", "wrkstat", "womenft", "degree", "partyid6", 
           "partyid7", "divlaw1")

by.year <- aggregate(sub[, vars2], list(year = sub$year), mean, na.rm = T)


# interpolate
by.year[30:40, "year"] <- c(1979, 1981, 1992, 1995, seq(1997, 2009, 2))
vars.omit <- c("wrkstat", "divlaw1", "babies", "preteen", "teens")
for(VAR in setdiff(vars2, vars.omit)){
  interpolate <- with(by.year, approx(year, get(VAR), method = "linear", xout = by.year[30:40, "year"]))
  by.year[30:40, paste(VAR)] <- interpolate$y
}

by.year <- arrange(by.year, year)
by.year <- subset(by.year, year <= 1992)
by.year <- mutate(by.year, 
                  repub = partyid6 + partyid7,
                  fulltime100 = fulltime*100,
                  repub100 = repub*100,
                  degreelt50100 = degreelt50*100)


# plots
with(by.year,{
  plot(NULL, ylim = c(0,3.5), xlim = range(year), type = "n", xlab = "year", ylab = "", bty = "l")
  points(year, fulltime, col = "navyblue", pch = 19)
  points(year, n.confinan, col = "maroon", pch = 19)
  legend("topright", c("fulltime", "n.confinan"), ncol = 2, pch = 19, col = c("navyblue", "maroon"))
})

# some practice with plotting & functions
plotByYear <- function(data, varname, color = "black", title = NULL, add = FALSE, ...){
  plotFun <- ifelse(add == FALSE, "plot", "lines")
  dd <- data[,c("year", varname)] 
  argsList <- list(x = dd, type = "l", bty = "l", lwd = 2, col = color, main = title, ...)
  do.call(plotFun, args = argsList)
}

plotByYear(by.year, "fulltime100", "purple4", "Pct working full-time")
plotByYear(by.year, "repub100", "orangered", "Pct strong republican")
plotByYear(by.year, "degreelt50100", "turquoise4", "Pct under 50 with BA+")


par(mfrow = c(3,1))
plot.vars <- c("fulltime100", "repub100", "degreelt50100")
plot.colors <- c("purple4", "orangered", "turquoise4")
for(i in 1:3){
  plotByYear(plot.vars[i], plot.colors[i], 
             data = by.year, 
             xlab = "", ylab = "", 
             title = plot.vars[i])
}

par(mfrow = c(1,1))
for(i in 1:3){
  ADD <- ifelse(i == 1, F, T)
  plotByYear(by.year, plot.vars[i], plot.colors[i], 
             ylab = "Percent", ylim = c(0,100), 
             add = ADD)
}
legend("top", legend = c("Work Full-Time", "Strong Republican", "<50 with BA"), 
       col = plot.colors, lwd = 2, bty = "n", seg.len = 1)


# correlations
cor.vars <- c("n.confinan", "fulltime100", "repub100", "degreelt50100", "year")
cor(by.year[, cor.vars], use = "complete")


# simplest regression
lm.confinan <- lm(n.confinan ~ fulltime100, data = by.year)
summary(lm.confinan)

# test for heteroskedasticity
bptest(lm.confinan)

# look for autocorrelation in errors
e <- lm.confinan$resid
acf(e) 
acf(e, xlim = c(1,8), col = "red", lwd = 2) # can also customize acf output
plot(e) # plot residuals over time
dwtest(lm.confinan) # Durbin-Watson test
bgtest(lm.confinan) # Breusch-Godfrey test
durbinWatsonTest(lm.confinan, max.lag=3) # Durbin-Watson with more lags


# include year trend
lm.confinan2 <- update(lm.confinan, ~ . + year)
summary(lm.confinan2)

# look for autocorrelation
e2 <- lm.confinan2$resid
acf(e2, xlim = c(1,8), col = "red", lwd = 2)
plot(e2)
dwtest(lm.confinan2)
bgtest(lm.confinan2)
durbinWatsonTest(lm.confinan2, max.lag=3)


# add some more predictores
lm.confinan3 <- lm(n.confinan ~ fulltime100 + repub100 + degreelt50100 + year, data = by.year)
summary(lm.confinan3)
vif(lm.confinan3) # variance inflation factor 
durbinWatsonTest(lm.confinan3, max.lag=2)


# We don't seem to have serial correlation here, but what if we did?
  # Solution 1: use Newey & West autocorrelation consistent covariance matrix estimator
coeftest(lm.confinan3, vcov = NeweyWest)

  # Solution 2: use the first differences
by.yearFD <- summarise(by.year,
                       n.confinan = firstD(n.confinan), # using firstD functon from QMSS package
                       fulltime100 = firstD(fulltime100),
                       repub100 = firstD(repub100),
                       degreelt50100 = firstD(degreelt50100),
                       year = year)

lm.confinan4 <- lm(n.confinan ~ fulltime100 + repub100 + degreelt50100 + year, data = by.yearFD)
summary(lm.confinan4)
e4 <- lm.confinan4$resid
plot(e4)
acf(e4, xlim = c(1,6), col = "red", lwd = 2)

  # Solution 3: Feasible GLS regression, i.e., Prais-Winston or Cochrane-Orcutt
# install.packages("orcutt")
library("orcutt")
cochrane.orcutt(lm.confinan3)