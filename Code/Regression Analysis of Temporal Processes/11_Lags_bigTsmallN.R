## to do:
    # figure out unit root test stuff for all examples
    # AR(1)-corrected fixed effects model
    # AR(1)-corrected 1st differences model



#============================================================================# 
#====== DISTRIBUTED LAG MODELS, LAGGED Y-VARS, 'BIG T, SMALL N' PANELS ======# 
#============================================================================# 

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





# Distributed lag models --------------------------------------------------
# _________________________________________________________________________

# For the US from 1983 to 1992 try to predict average frequency of prayer

vars <- c("attend", "pray", "year")
sub <- GSS[, vars]
sub$pray <- ReverseThis(sub$pray)

# get means by year
by.year <- aggregate(sub[,c("attend", "pray")], list(year = sub$year), mean, na.rm = T)

# interpolate for missing year 1992
by.year[30, "year"] <- 1992
by.year <- arrange(by.year, year)
by.year.ts <- ts(by.year)
by.year.ts <- na.approx(by.year.ts)



# only keep up 1983 to 1992
by.year.ts <- ts(by.year.ts[by.year.ts[,"year"] %in% 1983:1992,], start = 1983, end = 1992)

plot(by.year.ts[,c("attend", "pray")])
plot.dat <- meltMyTS(by.year.ts, time.var = "year")
ggMyTS(plot.dat) 


# correlations
cor(by.year.ts, use = "complete")

# simplest regression
lm.pray <- lm(pray ~ attend, data = by.year.ts)
summary(lm.pray)
acf(lm.pray$resid, ci.type = "ma", col = "darkgreen", lwd = 2)
dwtest(lm.pray)

# add trend
lm.pray2 <- update(lm.pray, ~ . + year)
summary(lm.pray2)
acf(lm.pray2$resid, ci.type = "ma", col = "darkgreen", lwd = 2)
dwtest(lm.pray2)


# Finite distributed lag process 
  # install.packages("dynlm")
library(dynlm)
dynlm(pray ~ L(attend, 0:1) + year, data = by.year.ts)
dynlm(pray ~ L(attend, 0:2) + year, data = by.year.ts)
dynlm(pray ~ L(attend, 0:3) + year, data = by.year.ts)

# The fading out of lags
coefs <- coef(dynlm(pray ~ L(attend, 0:3) + year, data = by.year.ts))
coefs
coefs <- coefs[-c(1,6)]
coefs <- round(coefs,2)
coefs
coef.names <- paste0("b_",0:3)
q_fading <- qplot(x = coef.names, coefs, geom="bar", 
                  stat="identity", fill = coef.names)
q_fading + ggtitle("The fading out of lags")

# Cumulative lags
coef.names <- paste0("c.b_",0:3)
q_cumulative <- qplot(x = coef.names, y = cumsum(coefs), geom="bar", 
                      stat="identity", fill = coef.names)
q_cumulative + ggtitle("Cumulative lags")

# Finite distributed lag process w/ diffs
dynlm(d(pray) ~ d(L(attend,0:3)), data = by.year.ts)

# check for unit root
library(fUnitRoots)




# Lagged dependent variables ----------------------------------------------
# _________________________________________________________________________

# load the time series from the marriage and education example
load("married_degree_TS.RData")
summary(by.year.ts)

# regression with lagged dependent variable (ldv) 
ldv.married <- dynlm(marriedlt50 ~ L(marriedlt50) + degreelt50 + year, data = by.year.ts)

# look for autocorrelation
qplot(x = 1973:1992, y = ldv.married$resid, geom = c("line","point")) + xlab("year") + ylab("residual")
bgtest(ldv.married)





# "Big T, Small N” panels -------------------------------------------------
# _________________________________________________________________________

vars <- c("year", "region", "sex", "age", "marital", "degree")
sub <- GSS[, vars]

# Recodes (using mutate from plyr, but could also use within(sub, ) )
sub <- mutate(sub, 
              married = ifelse(marital == 1, 1, 0),
              baplus = ifelse(degree >= 3, 1, 0),
              marriedlt50 = ifelse(married == 1 & age < 50, 1, 0),
              degreelt50 = ifelse(baplus == 1 & age <50, 1, 0))


# get means by year & region
by.year.region <- aggregate(subset(sub, sel = c(marriedlt50, degreelt50)), 
                            by = list(year = sub$year, region = sub$region), 
                            FUN = mean, na.rm = T)

# interpolate for some missing years
interp.dat <- expand.grid(year = c(1979, 1981, 1992), region = 1:9, 
                          marriedlt50 = NA, degreelt50 = NA)
by.year.region <- rbind(by.year.region, interp.dat)
by.year.region <- arrange(by.year.region, region, year)
for(i in 1:9){
  sel <- which(by.year.region$region == i)
  temp <- by.year.region[sel,]
  by.year.region[sel, ] <- na.approx(ts(temp))
}


# calculate pct under 50 married, under 50 with BA by year & region
by.year.region <- ddply(by.year.region, c("year", "region"), mutate,
                        marriedlt50_pct = 100*marriedlt50,
                        degreelt50_pct = degreelt50*100)

# only keep up to 1993
by.year.region <- subset(by.year.region, year <= 1993)

# plot overall trend with loess smoothing
g_mar.overall <- ggplot(by.year.region, aes(x = year, y = marriedlt50_pct)) 
g_mar.overall + stat_smooth(size = 2) 

# plot regional trends with loess smoothing
g_mar.region <- ggplot(by.year.region, aes(x = year, y = marriedlt50_pct, 
                                           group = region, color = factor(region))) 
g_mar.region + stat_smooth(se = F) 




plm.married <- plm(marriedlt50 ~ degreelt50 + as.numeric(year) + factor(region), 
                   model = "pooling", data = by.year.region)
clusterSE(plm.married, "region")
summary(plm.married)$fstatistic

# Wooldridge's test for serial correlation in FE panels
pwartest(marriedlt50 ~ degreelt50 + year, index = c("region", "year"), data = by.year.region)


# Levin-Lin-Chu test for unit root in all panels
rdat <- with(by.year.region, data.frame(split(marriedlt50, region)))
purtest(rdat, pmax = 1, test = "levinlin")
summary(purtest(rdat, pmax = 1, test = "levinlin"))


# Hadri test that no panels have unit root 
purtest(rdat, pmax = 1, exo = "trend", test = "hadri")


Panel.set <- plm.data(by.year.region, index = c("region", "year"))
library(tseries)
adf.test(Panel.set$marriedlt50, k=1)

library(fUnitRoots)
urersTest(by.year.region[which(by.year.region$region == 1), "marriedlt50"], type = "DF-GLS")

# can do it for each region using a loop
for(i in 1:9){
  test <- urersTest(by.year.region[which(by.year.region$region == i), "marriedlt50"], type = "DF-GLS")
  print(paste("Test for Region = ", i))
  print(test)
}



# first differences
library(rms)
by.year.region_FD <- ddply(by.year.region, "region", summarise,
                           marriedlt50 = firstD(marriedlt50),
                           degreelt50 = firstD(degreelt50),
                           region = region,
                           year = year)
with(by.year.region_FD, robcov(ols(marriedlt50 ~ degreelt50 + year, x = T)), cluster = region)


# fixed effects
fe.married <- plm(marriedlt50 ~ degreelt50 + as.numeric(year), index = c("region", "year"),
                   model = "within", data = by.year.region)
summary(fe.married)
sigmaRho(fe.married)


# random effects
re.married <- plm(marriedlt50 ~ degreelt50 + as.numeric(year), index = c("region", "year"),
                  model = "random", data = by.year.region)
summary(re.married)
sigmaRho(re.married)

# hausman test
phtest(fe.married, re.married)


# AR(1)-corrected fixed effects model

# AR(1)-corrected 1st differences model

