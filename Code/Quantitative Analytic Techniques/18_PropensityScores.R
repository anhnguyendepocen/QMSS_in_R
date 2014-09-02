#=======================================#
#====== PROPENSITY SCORE MATCHING ======#
#=======================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 


# Load the panel dataset 
load("GSS_panel.RData")


# Load packages
library(QMSS)



# Propensity Score Matching -----------------------------------------------
# _________________________________________________________________________

# Install Matching package
# install.packages("Matching")
library(Matching)

vars <- c("year", "happy", "marital", "born", "parborn", "region",
          "wordsum", "educ", "paeduc", "madeg", "incom16", "sex")

sub <- na.omit(pd[, vars])


# Recodes 
sub$n.happy <- ReverseThis(sub$happy)
sub$dm1 <- sub$marital == 1
sub$db1 <- sub$born == 1
sub$df1 <- sub$parborn == 0
sub$f.region <- factor(sub$region)

xvars <- c("dm1", "wordsum", "db1", "df1", "educ", 
           "paeduc", "madeg", "incom16", "f.region")
Formula <- as.formula(paste("n.happy ~ ", paste(xvars, collapse = " + ")))
Formula
lm(Formula, data = sub, sex == 1) # for men
lm(Formula, data = sub, sex == 2) # for women
lm(Formula, data = sub)           # overall


# Estimate the propensity model
xvars <- xvars[-1]
Formula <- as.formula(paste("dm1 ~ ", paste(xvars, collapse = " + ")))
propensity_model <- glm(Formula, data = sub, family = binomial)

# Matching & ATT estimate
  # outcome
Y <- sub$n.happy
  # treatment
Tr <- sub$dm1 
  # propensity scores
pscore <- propensity_model$fitted 
  # one-to-one matching
matching  <- Match(Y = Y, Tr = Tr, X = pscore)
summary(matching) # "Estimate" is the estimated ATT 

# Check/test for balance
mb <- MatchBalance(Formula, data = sub, match.out = matching, nboots = 500) 
