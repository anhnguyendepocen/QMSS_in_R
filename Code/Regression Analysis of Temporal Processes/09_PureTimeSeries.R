#==============================#
#====== PURE TIME SERIES ======#
#==============================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/24/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 


#load packages
library(QMSS)
library(gglot2)
library(plyr)
library(car)

# load data
load("GSS_panel.RData")
load("GSS.RData")




# A simple time series problem --------------------------------------------
# _________________________________________________________________________

### For the United States, from 1975-1992, try to predict average confidence in banks ###

vars <- c("cohort", "confinan", "attend", "pray", "conclerg", "homosex", "year", 
          "sex", "age", "realinc", "marital", "divlaw", "partyid", "polviews", 
          "babies", "preteen", "teens", "wrkstat", "divorce", "degree")

sub <- GSS[, vars]


# Recodes (using mutate from plyr, but could also use within(sub, ) )
sub <- mutate(sub, 
              n.confinan = ReverseThis(confinan), # ReverseThis from QMSS package
              kidslt18 = babies + preteen + teens,
              married = ifelse(marital == 1, 1, 0),
              baplus = ifelse(degree >= 3, 1, 0),
              fulltime = ifelse(wrkstat == 1, 1, 0),
              womenft = ifelse(sex == 2 & wrkstat == 1, 1, 0),
              marriedlt50 = ifelse(married == 1 & age < 50, 1, 0),
              degreelt50 = ifelse(baplus == 1 & age <50, 1, 0),
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

# interpolate for some missing years
# add the extra years
by.year[30:40, "year"] <- c(1979, 1981, 1992, 1995, seq(1997, 2009, 2))
by.year <- arrange(by.year, year)
# make a time series object by.year.ts
by.year.ts <- ts(by.year)
# interpolate using na.approx
by.year.ts <- na.approx(by.year.ts)

# calculate pct strong republican, percent fulltime, percent under 50 with BA
by.year.ts <- as.data.frame(by.year.ts)
by.year.ts <- mutate(by.year.ts, 
                     repub = partyid6 + partyid7,
                     repub_pct = repub*100,
                     fulltime_pct = fulltime*100,
                     degreelt50_pct = degreelt50*100)
# only keep up to 1992
?ts
by.year.ts <- ts(by.year.ts, start = 1, end = which(by.year.ts$year == 1992)) 




# Time series plots -------------------------------------------------------
# _________________________________________________________________________

install.packages("reshape2")

meltMyTS <- function(mv.ts.object, time.var, keep.vars){
  # mv.ts.object = a multivariate ts object
  # keep.vars = character vector with names of variables to keep 
  # time.var = character string naming the time variable
  require(reshape2)
  if(missing(keep.vars)) {
    melt.dat <- data.frame(mv.ts.object)
  }
  else {
    melt.dat <- data.frame(mv.ts.object)[, keep.vars]
  }
  melt.dat <- melt(melt.dat, id.vars = time.var)
  colnames(melt.dat)[which(colnames(melt.dat) == time.var)] <- "time"
  return(melt.dat)
}


keep.vars <- c("year", "n.confinan", "repub_pct","fulltime", 
               "fulltime_pct", "degreelt50_pct")
plot.dat <- meltMyTS(mv.ts.object = by.year.ts, 
                     time.var = "year", 
                     keep.vars = keep.vars)


ggMyTS <- function(df, varlist, ...){
  require(ggplot2)
  # varlist = character vector with names of variables to use
  include <- with(df, variable %in% varlist)
  gg <- ggplot(df[include,], aes(time, value, colour = variable)) 
  gg <- gg + geom_line(size = 1.25, ...) + geom_point(size = 3, ...)
  gg + theme(legend.position = "bottom")
} 

ggMyTS(df = plot.dat, varlist = c("fulltime", "n.confinan"))
ggMyTS(plot.dat, "n.confinan", color = "forestgreen") + ylab("Confidence in Banks")
ggMyTS(plot.dat, "fulltime_pct", color = "purple4") + ylab("Percent Full-time")
ggMyTS(plot.dat, "repub_pct", color = "orangered") + ylab("Percent Strong Republican")
ggMyTS(plot.dat, "degreelt50_pct", color = "royalblue") + ylab("Percent under 50 with BA")
ggMyTS(plot.dat, c("n.confinan", "fulltime_pct", "repub_pct", "degreelt50_pct"))




# correlations
cor.vars <- c("n.confinan", "fulltime_pct", "repub_pct", "degreelt50_pct", "year")
cor.dat <- data.frame(by.year.ts)[, cor.vars]
cor(cor.dat, use = "complete")

# install.packages("GGally")
library(GGally)
ggcorr(cor.dat, palette = "RdYlBu")
ggcorr(cor.dat, palette = "RdYlBu", label = T, label_round = 2)



# Heteroskedasticity & autocorrelation ------------------------------------
# _________________________________________________________________________

# simplest regression
lm.confinan <- lm(n.confinan ~ fulltime_pct, data = by.year.ts)
summary(lm.confinan)

# test for heteroskedasticity
bptest(lm.confinan)

# look for autocorrelation in errors
e <- lm.confinan$resid
acf(e) 
acf(e, xlim = c(1,8), col = "red", lwd = 2, ci.type = "ma") # can also customize acf output
plot(e) # plot residuals over time
dwtest(lm.confinan) # Durbin-Watson test
bgtest(lm.confinan) # Breusch-Godfrey test
durbinWatsonTest(lm.confinan, max.lag=3) # Durbin-Watson with more lags



# Stationarity ------------------------------------------------------------
# _________________________________________________________________________

# stationarity: mean
Lag.confinan <- lag(by.year.ts[,"n.confinan"])
mean.confinan <- rowMeans(cbind(Lag.confinan, by.year.ts[,"n.confinan"]))
mean.dat <- data.frame(year = by.year.ts[,"year"], mean = mean.confinan[-1])
(ggplot(mean.dat, aes(x=year, y=mean)) 
 + geom_line(color="forestgreen", size=1.25) 
 + geom_point(size = 3))

# stationarity: standard deviation
sd.confinan <- apply(cbind(Lag.confinan, by.year.ts[,"n.confinan"]),1,sd,na.rm=T)
sd.dat <- data.frame(year = by.year.ts[,"year"], sd = sd.confinan[-1])
ggplot(sd.dat, aes(x=year, y=sd)) + geom_bar(stat = "identity") 
ggplot(sd.dat, aes(x=year, y=sd)) + geom_bar(aes(fill = sd),stat = "identity") 



# Weak dependence ---------------------------------------------------------
# _________________________________________________________________________

par(mfrow = c(1,3))
varlist <- c("n.confinan", "degreelt50_pct", "repub_pct")
clrs <- c("forestgreen", "skyblue", "orangered")
for(i in 1:length(varlist)){
  acf(by.year.ts[,varlist[i]], lag.max = 8, na.action = na.omit, 
      col = clrs[i], lwd = 2, ci.type = "ma", main = varlist[i])
}
par(mfrow = c(1,1))

# always include year trend
lm.confinan2 <- lm(n.confinan ~ year + fulltime_pct, data = by.year.ts)
summary(lm.confinan2)

# look for autocorrelation in errors
e2 <- lm.confinan2$resid
acf(e2, xlim = c(1,8), col = "red", lwd = 2, ci.type = "ma") 
plot(e2) # plot residuals over time
dwtest(lm.confinan2) # Durbin-Watson test
bgtest(lm.confinan2) # Breusch-Godfrey test
durbinWatsonTest(lm.confinan2, max.lag=3) # Durbin-Watson with more lags

# add more predictors
lm.confinan3 <- update(lm.confinan2, ~ . + repub_pct + degreelt50_pct)
summary(lm.confinan3)
vif(lm.confinan3) # variance inflation factor 
durbinWatsonTest(lm.confinan3, max.lag=2)

# We don't seem to have serial correlation here, but what if we did?

  # Solution 1: use Newey & West autocorrelation consistent covariance matrix
  # estimator
coeftest(lm.confinan3, vcov = NeweyWest(lm.confinan3, lag = 1))

  # Solution 2: use the first differences
by.yearFD <- summarise(data.frame(by.year.ts),
                       n.confinan = firstD(n.confinan), # using firstD functon from QMSS package
                       fulltime_pct = firstD(fulltime_pct),
                       repub_pct = firstD(repub_pct),
                       degreelt50_pct = firstD(degreelt50_pct),
                       year = year)

lm.confinan4 <- update(lm.confinan3, data = by.yearFD)
summary(lm.confinan4)
e4 <- lm.confinan4$resid
plot(e4)
acf(e4, xlim = c(1,6), col = "red", lwd = 2, ci.type = "ma")

# Solution 3: Feasible GLS regression, i.e., Prais-Winston or Cochrane-Orcutt
# install.packages("orcutt")
library("orcutt")
cochrane.orcutt(lm.confinan3)
