# To-do so far:
# figure out how to print only some coefficients with summary(lm) for dummy var model



#====================================#
#====== FIXED & RANDOM EFFECTS ======#
#====================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load packages
library(QMSS)
library(plyr)
library(psych)
library(VGAM)

# load GSS panel data 
load("GSS_panel.RData")





# More First Differences --------------------------------------------------
# _________________________________________________________________________

# make reverse-coded version of "satfin" variable called "n.satfin"
Tab(pd$satfin)
pd$n.satfin <- ReverseThis(pd$satfin)
Tab(pd$n.satfin)
with(pd, table(satfin, n.satfin))


# make new variable "realinc10k" for family income in constant dollars in units of $10,000
pd$realinc10k <- pd$realinc/10^4

# make subset of data with needed variables for faster processing
pd.sub <- pd[,c("idnum","panelwave","n.satfin","realinc10k")]

### OLS WITH CLUSTERED STANDARD ERRORS ###
ols.satfin <- plm(n.satfin ~ realinc10k, data = pd.sub,
                  index = c("idnum", "panelwave"),
                  model = "pooling")
clusterSE(ols.satfin, cluster.var = "idnum", data = pd.sub)

### FIRST DIFFERENCES ###
fd.satfin <- plm(n.satfin ~ realinc10k + panelwave,
                 index=c("idnum", "panelwave"),
                 model="fd",
                 data=pd.sub)
summary(fd.satfin)





# From First Differences to Fixed Effects ---------------------------------
# _________________________________________________________________________

# make temporary dataset without obs for which panelwave is 3
temp <- pd.sub[-which(pd.sub$panelwave==3),] # or equivalently temp <- pd[which(pd$panelwave!=3),] 

# get ids for individuals without missingness on "n.satfin" and "realinc10k" for both waves 1 and 2
good1 <- with(pd.sub, idnum[which(!is.na(n.satfin) & !is.na(realinc10k) & panelwave==1)])
good2 <- with(pd.sub, idnum[which(!is.na(n.satfin) & !is.na(realinc10k) & panelwave==2)])
good_ids <- unique(c(good1,good2))

### First differences ###
fd.satfin2 <- plm(n.satfin ~ realinc10k + panelwave,
                  index = c("idnum", "panelwave"),
                  model = "fd",
                  data = subset(pd.sub, idnum %in% good_ids))
summary(fd.satfin2)


### Dummy variables model ###
dummy.satfin <- lm(n.satfin ~ realinc10k + panelwave + as.factor(idnum), 
                   data = subset(pd.sub, idnum %in% good_ids))
summary(dummy.satfin)$coef[1:3,] # don't print the nearly 2000 coefficients for the dummies

### Fixed effects model ###
fe.satfin <- plm(n.satfin ~ realinc10k + panelwave,
                 index=c("idnum", "panelwave"),
                 model="within", # set model = "within" for fixed effects
                 data=subset(pd.sub,subset=idnum %in% good_ids))
summary(fe.satfin)

#get standard deviation of individual fixed effects 
sd(fixef(fe.satfin))


### Fixed effects model for 3-wave panel ###
fe.satfin2 <- plm(n.satfin ~ realinc10k + panelwave,
                  index = c("idnum", "panelwave"),
                  model = "within",
                  data = pd.sub)
summary(fe.satfin2)




# Random effects vs. fixed effects ----------------------------------------
# _________________________________________________________________________

pd.sub <- pd[,c("idnum","panelwave","marital","marhomo","race","sex")]

#create binary variable "married" from recoding "marital"
pd.sub$married <- mapvalues(pd.sub$marital, from = 1:5, to = c(1,rep(0,4)))
Tab(pd.sub$married)

#create first-differenced variable "d.married" 
pd.sub <- ddply(pd.sub, .(idnum), mutate, d.married = firstD(married))
Tab(pd.sub$d.married)


#create first-differenced variable "d.marhomo" 
pd.sub <- ddply(pd.sub, .(idnum), mutate, d.marhomo=firstD(marhomo))
Tab(pd.sub$d.marhomo)

### Naive cross-sectional OLS with clustered standard errors ###
ols.marhomo <- plm(marhomo ~ married + panelwave, data = pd.sub,
                   index = c("idnum", "panelwave"),
                   model = "pooling")
clusterSE(fit = ols.marhomo, cluster.var = "idnum", data = pd.sub)

### Fixed effects ###
fe.marhomo <- plm(marhomo ~ married + panelwave,
                  index = c("idnum", "panelwave"),
                  model = "within",
                  data = pd.sub)
summary(fe.marhomo)

### Random effects ###
re.marhomo <- plm(marhomo ~ married + panelwave,
                  index = c("idnum", "panelwave"),
                  model = "random", # set model = "random" for random effects
                  data = pd.sub)
summary(re.marhomo)
ercomp(re.marhomo)

### Hausman test ###
phtest(fe.marhomo, re.marhomo)


### Bigger random effects model ###
#create indicator variable for race==black
pd.sub$black <- pd.sub$race==2

re.marhomo2 <- plm(marhomo ~ married + panelwave + sex + black,
                   index=c("idnum", "panelwave"),
                   model="random",
                   data=pd.sub)
summary(re.marhomo2)


### First differences ###
pd.sub$panelwave3 <- pd.sub$panelwave==3
fd.marhomo <- plm(d.marhomo ~ d.married + panelwave3, data = pd.sub,
                  index = c("idnum", "panelwave"),
                  model = "pooling")
summary(fd.marhomo)
clusterSE(fit = fd.marhomo, cluster.var = "idnum", data = pd.sub)
