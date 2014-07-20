#=============================================#
#====== MULTINOMIAL LOGISTIC REGRESSION ======#
#=============================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load packages
library(QMSS)
library(ggplot2)
library(nnet)
library(plyr)
library(psych)
library(VGAM)
library(visreg)

# Load the cumulative GSS and the GSS_2010 dataset 
load("GSS.RData")
load("GSS_2010.RData")



# Multinomial Logit -------------------------------------------------------
# _________________________________________________________________________

vars <- c("natchld", "marital", "realinc", "age", "polviews", "childs")
sub <- GSS[, vars]
sub$married <- sub$marital == 1
sub$childcare <- factor(sub$natchld,
                        levels = c(3,2,1),
                        labels = c("too much", "about right", "too little"),
                        ordered = T)

# fit multinomial logit model using multinom() from nnet package
multinom.child <- multinom(childcare ~ childs + age + married + I(log(realinc)) 
                           + polviews, data = sub)
summary(multinom.child)

# unfortunatley it doesn't report z-values or p-values but we can compute them
b <- summary(multinom.child)$coefficients
se <- summary(multinom.child)$standard.errors
p.vals <- 2*(1 - pnorm(abs(b/se)) ) # compute p-values (2-tailed test), z = b/se
p.vals

# Extract the relative risk ratios
exp(coef(multinom.child)[,-1]) # [,-1] to get rid of the first column (the intercepts)


# Predicted probabilities after multinomial logit -------------------------
# _________________________________________________________________________

# data frame of values to use for predictions
data.child <- expand.grid(
  age = 20:80, # let age vary from 20 to 80 
  polviews = 1:7,  # let polviews take on each of its 7 
  childs = mean(sub$childs, na.rm = T), # fix childs at its mean
  married = TRUE, # fix married as TRUE
  realinc = mean(sub$realinc,na.rm=T)) # fix realinc at its mean


# combine polviews and predicted probabilities in a data frame
preds.child <- data.frame( 
  polviews = data.child$polviews, # polviews
  predict(multinom.child, newdata = data.child, type = "probs", se = TRUE)) # predicted probabilities

# avg predicted probabilities for each level of polviews
ddply(preds.child, "polviews", colMeans) 




# What predicts owning a pet and a specific kind of pet? ------------------
# _________________________________________________________________________

library(foreign)
pets <- read.dta("Data/04489-0001-Data-Pets.dta")

# Recoding
pets$pet <- with(pets, ifelse(Q68 == "Yes", Q69, 0))
pets$pet[pets$pet == 5] <- NA
pets$pet <- factor(pets$pet,
                   labels = c("None","Dog(s)", "Cat(s)", "Dog & Cat", "Other"))
pets$kids <- mapvalues(as.numeric(pets$KIDS),
                       from = 1:3,
                       to = c(1, 0, NA))
pets$app_clinton <- mapvalues(as.numeric(pets$CLINTON),
                              from = 1:3,
                              to = c(1, 0, NA))
pets$sex <- as.numeric(pets$SEX)
save(pets, file = "pets.RData")

table(pets$pet)
describe(pets[,c("kids", "app_clinton", "sex")])

# Multinomial logit model
multinom.pets <- multinom(pet ~ app_clinton + sex + kids, data = pets)
summary(multinom.pets)

local({ #it doesn't report p-values but we can compute them
  b <- summary(multinom.pets)$coefficients
  se <- summary(multinom.pets)$standard.errors
  z <- b/se
  p.vals <- 2*(1 - pnorm(abs(z)) ) # compute p-values (2-tailed test)
  round(p.vals, 3)
})


# Predicted probabilities 
describe(predict(multinom.pets, type = "probs"))

# Predicted probabilities for Clinton-hating woman with kids and Clinton-loving man with no kids
preds.pets <- predict(multinom.pets, type = "probs", 
                      newdata = data.frame(app_clinton = c(0,1), kids = c(1,0), sex = c(2,1)))
rownames(preds.pets) <- c("Clinton-hating woman with kids:",
                          "Clinton-loving man with no kids:")
print(preds.pets, digits = 3)
