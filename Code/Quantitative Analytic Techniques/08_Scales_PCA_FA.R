#==========================================#
#====== SCALES, PCA, FACTOR ANALYSIS ======#
#==========================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014


# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# Set working directory
setwd("/Users/jgabry/Desktop/COLUMBIA/Stuff_for_Greg")

# Load some packages
library(QMSS)
library(ggplot2)
library(memisc)
library(plyr)
library(visreg)

# Load the cumulative GSS and the GSS_2010 dataset 
load("GSS.RData")
load("GSS_2010.RData")




# Scales ------------------------------------------------------------------
# _________________________________________________________________________

vars <- c("reborn", "relexp", "attend", "fund", "reliten", "relneg", "relpersn", "pray", "punsin")
sub <- GSS_2010[, vars]

# Get correlations between all variables 
cor(sub, use = "complete.obs") 

# Cronbach's alpha
library(psych)
summary(alpha(sub)) 
alpha(sub) # can also get more detailed output than summary(alpha(sub)) 

# Multiple regression using all variables
sub <- cbind(polviews = GSS_2010$polviews, sub) # add polviews to sub
lm.polviews <- lm(polviews ~ reborn + relexp + attend + fund + reliten + relneg + 
                    relpersn + pray + punsin, data = sub)
summary(lm.polviews)
stdCoef(lm.polviews) # standardized coefficients using our custom function

# Variance inflation factors
library(car)
vif(lm.polviews)

# Make Scale
  # first reverse code some variables 
    # for reversing a lot of variables at once we can use reverse.code() from psych package
keys <- c(1,-1,-1, 1,-1,-1, 1,-1,-1,-1) # vector with entry for each var in sub (-1 indicates that var is to be reverse coded) 
sub <- as.data.frame(reverse.code(keys = keys, items = sub)) # reverse code the desired variables
  # create scale by standardizing and taking row mean
sub$religion <- rowMeans(scale(sub[,-1])) # use sub[,-1] to exclude the first column "polviews"

summary(lm(polviews~religion, data = sub))
stdCoef(lm(polviews~religion, data = sub))


# More on standardization
alpha <- summary(alpha(GSS_2010[,c("educ","realinc","prestg80")]))
alpha$raw_alpha # unstanderdized (very low alpha)
alpha$std.alpha # standardized (much higher alpha)




# Principal Components Analysis (PCA) & Factor Analysis -------------------
# _________________________________________________________________________


# Variables about confidence in institutions
sub <- subset(GSS, select = c(confinan:conarmy))

# Reverse code all of the variables
keys <- rep(-1, ncol(sub)) # vector of -1s with length equal to the number of columns in our subset 
sub <- as.data.frame(reverse.code(keys = keys, items = sub)) # reverse code all variables

# Cronbach's alpha
alpha(sub)

# PCA
pca <- prcomp(na.omit(sub), scale=T)
summary(pca)
pca # more detailed output than summary (gives individual loadings)

pca$rotation # extract eigenvectors
pca$sdev # extract square root of the eigenvalues
pca$rotation # extract factor loadings
pca$x # extract principal components


# Factor analysis
install.packages("GPArotation")
  # using fa() function from psych package
fa <- fa(sub, nfactors=5, fm = "pa", rotate="none")
fa <- fa(sub, nfactors=5, fm = "pa", rotate="none", max.iter = 100) # might need to increase max number of iterations
fa

  # varimax rotation
fa.rotate <- fa(sub, nfactors = 5, fm = "pa", rotate = "varimax", max.iter = 100)
fa.rotate$loadings
factors <- fa.rotate$scores[,1:3] # extract factors
colnames(factors) <- c("fed", "theman", "media")
describe(factors)
cor(factors, use = "complete.obs")

  # regression model using the factors
sub <- cbind(sub, subset(GSS, select = c(polviews, age, sex, educ, race, marital)))
sub <- cbind(sub, factors)
summary(lm(polviews ~ fed + theman + media + age + educ + as.factor(sex) 
           + as.factor(race) + as.factor(marital), data = sub))

# Low-tech factor creation
sub$r.fed <- rowMeans(sub[,c("confed-", "conjudge-", "conlegis-")])
sub$r.media <- rowMeans(sub[,c("conpress-", "contv-")])
sub$r.theman <- rowMeans(sub[,c("confinan-", "conbus-", "conclerg-", "conarmy-")])
summary(lm(polviews ~ r.fed + r.media + r.theman + age + educ + as.factor(sex) 
           + as.factor(race) + as.factor(marital), data = sub))

