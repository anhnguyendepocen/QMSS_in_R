#=============================================#
#====== CORRELATION & LINEAR REGRESSION ======#
#=============================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014





# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load some packages
library(QMSS)
library(plyr)

# Load the previously saved 2010 subset of the GSS
load("GSS_2010.RData")




# Slope & Intercept -------------------------------------------------------
# _________________________________________________________________________


### Job prestige & educational attainment ###

# Take the subset of GSS_2010 without missings on educ and prestg80 and with educ at least 10 (for demonstration purposes)
sub <- subset(GSS_2010, educ>=10 & !is.na(educ) & !is.na(prestg80))
# Take a random sample of 10 individuals (for demonstration purposes) and display and plot their values for educ & prestg80
samp <- sub[sample(nrow(sub),10), c("id", "educ", "prestg80")]
samp <- arrange(samp, educ, prestg80, id)  # sort by educ then prestg80 then id       
samp
with(samp, plot(educ, prestg80, # the x and y values of the points to plot
                pch = 19, # pch controls the plotting ‘character’ (the symbol used). see ?points for more details
                col="skyblue")) # color the points "skyblue"


# What do we need to calculate the slope?
Xbar <- mean(samp$educ)
Ybar <- mean(samp$prestg80)
tab <- ddply(samp, "id", summarise,
             X = educ,
             Y = prestg80,
             Xbar = Xbar,
             Ybar = Ybar,
             XminusXbar = X - Xbar,
             YminusYbar = Y - Ybar,
             "(X-Xbar)(Y-Ybar)" = XminusXbar*YminusYbar,
             "(X-Xbar)^2" = XminusXbar^2,
             "(Y-Ybar)^2" = YminusYbar^2)

tab <- arrange(tab, X, Y, id)
print(tab, row.names=F)


# Calculate the slope & intercept 
numerator <- sum(tab[,"(X-Xbar)(Y-Ybar)"])
denominator <- sum(tab[,"(X-Xbar)^2"])
slope <- numerator/denominator
slope
intercept <- Ybar - slope*Xbar
intercept
  
# scatterplot with regression line extended beyond the data
  # plot the points
with(samp, plot(educ, prestg80, 
                xlim = c(0, max(samp$educ) + 10), # entend x-axis past the maximum educ value by 10
                ylim = c(0, intercept + slope*(max(educ) + 10)), # extend the y-axis 
                pch = 19, 
                col = "skyblue")) 
  # add the line with our calculated slope & intercept
abline(a = intercept, b = slope, 
       lwd = 2, # lwd controls the line width 
       lty = 2, # lwd controls the line type (lty = 2 will produce a dashed line)
       col = "maroon") 
  # add some arrows to emphasize that the regression line continues outside the range of the data
with(samp, arrows(
  x0 = range(educ), # x-coordinates of points FROM which to draw.
  y0 = intercept + slope*range(educ), # y-coordinates of points FROM which to draw.
  x1 = c(0, max(educ) + 10), # x-coordinates of points TO which to draw.
  y1 = intercept + slope*c(0, max(educ) + 10), # y-coordinates of points TO which to draw.
  length = 0.2, # length of the edges of the arrow head (in inches)
  lwd = 2, 
  col = "maroon"))



# Correlation & Linear Regression -----------------------------------------
# _________________________________________________________________________


# Simple Linear Regression Output
summary(lm(spanking ~ degree, data = GSS_2010))
  
  # Or we can assign the fitted model to an object
lm.prestige <- lm(prestg80 ~ educ, data=samp)
lm.prestige
summary(lm.prestige)

  # We can get the fitted values (yhat) 
Yhat <- lm.prestige$fitted
Yhat

  # we can extract a single coefficient using coef()
coef(lm.prestige)["educ"] # or equivalently lm.prestige$coef["educ"]
coef(lm.prestige)[["educ"]] # using the double brackets [[ will drop the name of the selected element 

# Correlation
  # r = b*sx/sy
sx <- sd(tab$X) 
sy <- sd(tab$Y)
b <- coef(lm.prestige)[["educ"]] 
r <- b * sx/sy
r

  # Or use cor() function
with(samp, cor(educ, prestg80))


# R-Squared (Coefficient of Determination)
  # R^2 = (SST - SSE)/SST
SST <- with(tab, sum((Y-Ybar)^2))
SSE <- with(tab, sum((Y - Yhat)^2))
R2 <- (SST - SSE)/SST
R2

  # or R^2 = r*r
with(samp, cor(educ, prestg80)^2)

  # or Extract R^2 from lm.prestige
summary(lm.prestige)$r.squared


# T-scores
  # t = b/se
summary(lm.prestige)$coef[, "Estimate"]
summary(lm.prestige)$coef[,"Std. Error"]
t <- summary(lm.prestige)$coef[, "Estimate"]/summary(lm.prestige)$coef[,"Std. Error"]
t

  # or extract t from lm.prestige
summary(lm.prestige)$coef[, "t value"] # or summary(lm.prestige)$coef[, 3]


# Confidence intervals for model parameters
confint(lm.prestige, "educ") # 95% interval
confint(lm.prestige, "educ", level = 0.99) # 99% interval

# T-test for correlation
with(samp, cor.test(educ, prestg80)) # default confidence level is 95%



### Predicting spanking with degree ###

# OLS model
summary(lm(spanking ~ degree, data = GSS_2010))


### Predicting TV-watching hours with race (using dummy variable) ###

# Make dummy variable for race=white (white is coded as 1)
Tab(GSS_2010$race)
GSS_2010$white <- GSS_2010$race == 1
Tab(GSS_2010$white)
with(GSS_2010, table(white, race))

# OLS model
summary(lm(tvhours ~ white, data = GSS_2010))
