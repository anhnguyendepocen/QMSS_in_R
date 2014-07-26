#=======================================================================#
#====== LAGS, 'BIG T, SMALL N', CO-INTEGRATION, GRANGER CAUSALITY ======#
#=======================================================================#

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
load("GSS_panel.RData")
load("GSS.RData")





# Distributed lag models --------------------------------------------------
# _________________________________________________________________________

# For the US from 1983 to 1992 try to predict average frequency of prayer

vars <- c("attend", "pray", "year")
sub <- GSS[, vars]
sub$pray <- ReverseThis(sub$pray)

# get means by year
by.year <- aggregate(sub[,c("attend", "pray")], list(year = sub$year), mean, na.rm = T)

# interpolate for some missing years
by.year[30,"year"] <- 1992
by.year <- arrange(by.year, year)
by.year.ts <- ts(by.year)
by.year.ts <- na.approx(by.year.ts)

# only keep up 1983 to 1992
by.year.ts <- by.year.ts[by.year.ts[,"year"] %in% 1983:1992,]
plot.dat <- meltMyTS(by.year.ts, time.var = "year")
ggMyTS(df = plot.dat) + theme(axis.text.x = element_text(angle = 90))  
# correlations
cor(by.year.ts, use = "complete")

# simplest regression
by.year.ts <- data.frame(by.year.ts)
lm.pray <- lm(pray ~ attend, data = by.year.ts)
summary(lm.pray)
acf(lm.pray$resid, ci.type = "ma", col = "darkgreen", lwd = 2)
durbinWatsonTest(lm.pray)


library(scales)
?ur.ers
?embed
help(package = "fUnitRoots")
theme(axis.text.x = element_text(angle = 90))  

