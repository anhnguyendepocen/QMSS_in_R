#=========================================#
#====== ORDINAL LOGISTIC REGRESSION ======#
#=========================================#

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
library(VGAM)

# Load the cumulative GSS and the GSS_2010 dataset 
load("GSS.RData")
load("GSS_2010.RData")




# Ordinal logistic regression  --------------------------------------------
# _________________________________________________________________________


### What predicts if you support more government spending on childcare ###

vars <- c("natchld", "marital", "realinc", "age", "polviews", "childs")
sub <- GSS[, vars]

sub$married <- sub$marital == 1

Tab(sub$natchld)
sub$childcare <- factor(sub$natchld,
                        levels = 3:1,
                        labels = c("too much", "about right", "too little"),
                        ordered = TRUE) # tell R that there is an order to the levels
Tab(sub$childcare)


# Ordinal logit model using vglm() from VGAM package
vglm.child <- vglm(childcare ~ childs + age + married + I(log(realinc)) + polviews, 
                   data = sub, family = propodds)
summary(vglm.child) 

# the output doesn't give p-values, but we can compute them and add them to the output
coef.child <- coef(summary(vglm.child))
coef.child <- data.frame(coef.child)
coef.child[, "p.value"] <- 2*(1 - pnorm(abs(coef.child[, "z.value"]))) # compute p-values (2-tailed test)


# can also add odds ratios to the output 
coef.child$odds.ratio <- exp(coef.child[, "Estimate"]) 
coef.child




# Ordinal regression via multiple binary logistic regressions  ------------
# _________________________________________________________________________

sub$childcare_23 <- with(sub, childcare == "about right" | childcare == "too little")
sub$childcare_3 <- sub$childcare == "too little"
logit.23_vs_1 <- glm(childcare_23 ~ childs + age + married + I(log(realinc)) + polviews, 
                  data = sub, family = binomial)
summary(logit.23_vs_1)
logit.12_vs_3 <- glm(childcare_3 ~ childs + age + married + I(log(realinc)) + polviews, 
                     data = sub, family = binomial)
summary(logit.12_vs_3)
  
# compare the coefficients
coeff.table <- cbind("Logit (2,3 vs. 1)" = coef(logit.23_vs_1)[-1], #[-1] removes the intercept
                "Logit (1,2 vs. 3)" = coef(logit.12_vs_3)[-1], #[-1] removes the intercept 
                "Ordinal Logit" = coef(vglm.child)[-c(1,2)]) # [-c(1,2)] removes the two intercepts/cutpoints
print(coeff.table, digits = 3)

# What if we did OLS? 
sub$n.childcare <- as.numeric(sub$childcare)
summary(lm(n.childcare ~ childs + age + married + I(log(realinc)) + polviews, data = sub))
# arbitrary category distance changes OLS estimates
sub$n.childcare <- mapvalues(sub$n.childcare, from = 1:3, to = c(1, 45, 46))
summary(lm(n.childcare ~ childs + age + married + I(log(realinc)) + polviews, data = sub))
# but the ordinal model still gives the original results even with the arbitrary category distances
summary(vglm(
  factor(n.childcare, ordered = T) ~ childs + age + married + I(log(realinc)) + polviews, 
  data = sub, family = propodds))

 
# Relaxing and testing the proportion odds assumption ---------------------
# _________________________________________________________________________

# To relax prop odds assumption put family = cumulative(reverse=T) instead of family = propodds
vglm.child2 <- vglm(childcare ~ childs + age + married + I(log(realinc)) + polviews,
                    data = sub, family = cumulative(reverse=T)) 
summary(vglm.child2)

# Test the proportional odds assumption (I don't know of a good function for this but we can 
#   write our own). The arguments "fit" and "relaxed.fit" will be vglm models with family = propodds 
#   and family = cumulative(reverse=T), respectively
propOddsTest <- function(fit, relaxed.fit){ 
  dev1 <- deviance(fit); dev2 <- deviance(relaxed.fit) # model deviances (deviance =  -2*log-likelihood ratio)
  df.res1 <- df.residual(fit); df.res2 <- df.residual(relaxed.fit) # degrees of freedom
  chi.sq <- dev1 - dev2 # the chi-squared statistic is the the difference of the deviances
  df <- df.res1 - df.res2 # the df for the chi-square distribution is the difference of the df
  p.val <- pchisq(chi.sq, df, lower.tail = F) # compute p-value using chi-squared distribution
  return(list(message = "Null hypothesis: no violation of assumption.",
              chi.sq = chi.sq, 
              df = df, 
              p.val = p.val))
}

# Test the assumption using our prop.odds.test function
propOddsTest(vglm.child, vglm.child2)

# In the future propOddsTest is also in the QMSS package
?propOddsTest


# Predicted probabilities after ordinal logistic regression ---------------
# _________________________________________________________________________

Tab(GSS_2010$happy)
GSS_2010$n.happy <- ReverseThis(GSS_2010$happy) # reverse code using custom function

GSS_2010$n.happy <-  factor(GSS_2010$n.happy, 
                            labels = c("not happy","pretty happy","very happy"),
                            ordered=T) # make ordered factor var

Tab(GSS_2010$n.happy)

vglm.happy <- vglm(n.happy ~ degree, data = GSS_2010, family = propodds)
summary(vglm.happy)

# make data frame of values of degree to use for the predictions
pred.dat <- data.frame( 
  degree = c(
    range(GSS_2010$degree, na.rm = T), # min and max values of degree
    mean(GSS_2010$degree, na.rm = T) + c(-1,1)*sd(GSS_2010$degree, na.rm = T), # 1 SD below and 1 SD above the mean
    mean(GSS_2010$degree, na.rm = T) + c(-2,2)*sd(GSS_2010$degree, na.rm = T) # 2 SDs below and 1 SDs above the mean
  ))
pred.dat

# replace with the predict probabilities
pred.dat <- predict(vglm.happy, type = "response", newdata = pred.dat) 
pred.dat

# use these predictions to compute changes in predicted probabilities as degree changes from  
# its min to max, mean-sd to mean+sd, and mean-2sd to mean+2sd 
pred.dat <- rbind("min -> max" = pred.dat[2,]-pred.dat[1,], # change from min to max
                  "mean-sd -> mean+sd" = pred.dat[4,]-pred.dat[3,], # change from 1 SD below the mean to 1 SD above 
                  "mean-2sd -> mean+2sd" = pred.dat[6,]-pred.dat[5,]) # change from 2 SDs below the mean to 2 SDs above 
print(pred.dat, digits = 4)






