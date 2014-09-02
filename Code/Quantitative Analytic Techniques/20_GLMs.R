#=======================================#
#====== GENERALIZED LINEAR MODELS ======#
#=======================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3





# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# load packages
library(QMSS)
library(ggplot2)


# Load the cumulative GSS dataset 
load("GSS.RData")




# Poisson & Negative Binomial ---------------------------------------------
# _________________________________________________________________________

### What explains R's number of female sex partners since age 18? ###
vars <- c("numwomen", "sex", "age", "year", "marital", "id")
sub <- GSS[, vars]

summary(sub$numwomen)
keep <- which(sub$numwomen < 990)
sub <- sub[keep, ]
summary(sub$numwomen)

# histogram of numwomen
qplot(sub$numwomen) # qplot is a quick way to make a plot, letting ggplot choose the default geom (plot type)


# start with OLS
lm.numwomen <- lm(numwomen ~ as.factor(sex) + age + year + as.factor(marital), 
                  data = sub)
summary(lm.numwomen)

# summary of the in-sample predictions (i.e. fitted values)
summary(lm.numwomen$fitted) # we get negative counts and a low max

# Poisson regression with glm() with family = poisson (the default link is
# "log"))
pois.numwomen <- glm(numwomen ~ as.factor(sex) + age + year + as.factor(marital), 
                     data = sub, family = poisson) 
summary(pois.numwomen)$coef
exp(coef(pois.numwomen)) # exponentiated coefficients

# can retest with robust standard errors
library(sandwich)
coeftest(pois.numwomen, vcov = vcovHC(pois.numwomen, type = "HC0"))


# predicted count for a married person of mean age in the mean year of the
# survey by gender
predict(pois.numwomen, type = "response", 
        newdata = data.frame(
          year = mean(sub$year, na.rm = T),
          age = mean(sub$age, na.rm = T), 
          marital = 1, 
          sex = 1:2)) # get predictions for sex==1 and sex==2


# With exposure (use offset argument of glm())
pois.numwomen2 <- glm(numwomen ~ as.factor(sex) + year + as.factor(marital), 
                     data = sub, family = poisson, 
                     offset = log(age))
summary(pois.numwomen2)
coeftest(pois.numwomen2, vcov = vcovHC(pois.numwomen2, type = "HC0"))


# Compare variance & mean of outcome to look for overdispersion (Poisson random
# variable has mean = variance)
mean(sub$numwomen, na.rm = T)
var(sub$numwomen, na.rm = T)


# Negative binomial regression (with glm.nb() from MASS package)
negbin.numwomen <- glm.nb(numwomen ~ as.factor(sex) + year + age 
                          + as.factor(marital), data = sub)
summary(negbin.numwomen)




# Gamma -------------------------------------------------------------------
# _________________________________________________________________________
vars <- c("realinc", "age", "marital", "educ")
sub <- GSS[, vars]

# OLS
lm.realinc <- lm(realinc ~ age + I(age^2) + as.factor(marital) + educ, data = sub)
summary(lm.realinc)

# Gamma regression
gamma.realinc <- glm(realinc ~ age + I(age^2) + as.factor(marital) + educ, 
                      data = sub, family = Gamma(link = "identity"))
summary(gamma.realinc)


# OLS with logged dependent variable
lm.realinc2 <- lm(I(log(realinc)) ~ age + I(age^2) 
                  + as.factor(marital) + educ, data = sub)

# Gamma regression with log link function
gamma.realinc2 <- glm(realinc ~ age + I(age^2) + as.factor(marital) + educ, 
                     data = sub, family = Gamma(link = "log"))

# Compare coefficients
  # make a list of the models
models <- list("OLS" = lm.realinc, "Gamma(identity)" = gamma.realinc,
               "OLS(log)" =  lm.realinc2, "Gamma(log)" = gamma.realinc2)
  # apply the coef function to each model in the models list
coef.comparison <- sapply(X = models, FUN = coef)
round(coef.comparison, 3)
