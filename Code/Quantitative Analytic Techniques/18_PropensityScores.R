#=======================================#
#====== PROPENSITY SCORE MATCHING ======#
#=======================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014


# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# Set working directory
setwd("/Users/jgabry/Desktop/COLUMBIA/Stuff_for_Greg")


# Load the panel dataset 
load("GSS_panel.RData")

# Install Matching package
# install.packages("Matching")

# Load packages
library(QMSS)
library(Matching)





# Propensity Score Matching -----------------------------------------------
# _________________________________________________________________________

vars <- c("year", "happy", "marital", "born", "parborn", "region",
          "wordsum", "educ", "paeduc", "madeg", "incom16", "sex")

sub <- na.omit(pd[, vars])


# Recodes 
sub$n.happy <- ReverseThis(sub$happy)
sub$dm1 <- sub$marital == 1
sub$db1 <- sub$born == 1
sub$df1 <- sub$parborn == 0
sub$f.region <- factor(sub$region)

xvars <- c("dm1", "wordsum", "db1", "df1", "educ", "paeduc", "madeg", "incom16", "f.region")
Formula <- as.formula(paste("n.happy ~ ", paste(xvars, collapse = " + ")))
Formula
lm(Formula, data = sub, sex == 1) # for men
lm(Formula, data = sub, sex == 2) # for women
lm(Formula, data = sub)           # overall


# Estimate the propensity model
xvars <- xvars[-1]
Formula <- as.formula(paste("dm1 ~ ", paste(xvars, collapse = " + ")))
glm1 <- glm(Formula, data = sub, family = binomial)

# Matching & ATT estimate
X <- glm1$fitted # propensity score
Y <- sub$n.happy # outcome
Tr  <- sub$dm1 # treatment
match.out  <- Match(Y = Y, Tr = Tr, X = X, M = 1) # one-to-one matching with replacement (the "M=1" option).
summary(match.out) # "Estimate" is the estimated ATT 

# Check for balance
mb <- MatchBalance(Formula, data = sub, match.out = match.out, nboots = 500) 

