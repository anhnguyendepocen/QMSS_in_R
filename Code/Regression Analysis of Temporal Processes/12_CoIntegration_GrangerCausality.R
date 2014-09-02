#===============================================# 
#====== CO-INTEGRATION, GRANGER CAUSALITY ======# 
#===============================================#

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



# Co-integration ----------------------------------------------------------
# _________________________________________________________________________


# load the time series from the marriage and education example
load("married_degree_TS.RData")
summary(by.year.ts)


# step 1: run a regression
lm.married <- lm(marriedlt50 ~ degreelt50, data = by.year.ts)
summary(lm.married)

# steps 2 & 3: get the errors and then run a unit root test on them
e <- lm.married$resid

library(tseries)
adf.test(e)

# Johansen test
library(urca)
johansen <- ca.jo(by.year.ts[,c("marriedlt50", "degreelt50")], K = 2)
summary(johansen) # lots of info
cbind(teststat = johansen@teststat, johansen@cval) # just look at test statistics and critical values




# Granger causality -------------------------------------------------------
# _________________________________________________________________________

# Granger test (using grangertest from lmtest package)
  # specify y ~ x, order or lags, and data
  # it will automatically run the lagged models needed for the test
grangertest(marriedlt50 ~ degreelt50, order = 3, data = by.year.ts)
grangertest(degreelt50 ~ marriedlt50, order = 3, data = by.year.ts)


# Using vector autoregressive model
  # install.packages("vars")
library(vars)

var.married <- VAR(by.year.ts[,c("marriedlt50", "degreelt50")], p = 2)
summary(var.married)

causality(var.married, cause = "marriedlt50")$Granger
causality(var.married, cause = "degreelt50")$Granger