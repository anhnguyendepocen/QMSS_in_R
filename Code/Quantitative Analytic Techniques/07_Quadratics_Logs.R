#===================================================#
#====== QUADRATIC TERMS & LOG TRANSFORMATIONS ======#
#===================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014


# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load some packages
library(QMSS)
library(ggplot2)
library(plyr)


# Load the cumulative GSS and the GSS_2010 dataset 
load("GSS.RData")
load("GSS_2010.RData")


# Quadratic terms ---------------------------------------------------------
# _________________________________________________________________________


### Predicting anger ###

sub <- GSS[, c("angry", "sei")]

# Linear regression
lm.angry <- lm(angry ~ sei, data = sub)
summary(lm.angry)

# Curvilinear Regression
lm.angry2 <- lm(angry ~ sei + I(sei^2), data = sub)
summary(lm.angry2)

# Visualizing the curvilinear relationship
  # with ggplot()
g <- ggplot(sub, aes(x = sei, y = angry)) + geom_point() # scatterplot
g
g + geom_smooth(method = "lm", formula = y ~ poly(x, 2)) # add fitted quadratic line

  # or manually with traditional R plotting functions
    # first plot the points
with(sub, plot(sei, angry, pch = 20, cex = 0.8, col = "darkred"))
    # now add the curve using the coefficients from our regression 
b <- round(lm.angry2$coef, 4)
curve(b %*% rbind(1,x,x^2), add=T, lwd = 2, col = "darkblue")  # or equivalently b[1] + b[2]*x + b[3]*x^2 instead of b%*%rbind(1,x,x^2)

# Omitted variables test (using resettest() from lmtest package)
?resettest
resettest(lm.angry, power = 2:4)



### Do Republicans go to religious services more often than Democrats? ###

sub <- GSS[, c("attend", "partyid")]
with(sub, Tab(partyid))

# Linear regression
lm.attend <- lm(attend ~ partyid, data = sub, partyid < 7)
summary(lm.attend)

# Basic barplot of mean religious attendance for each partyid
model.dat <- lm.attend$model # get only the obs used in fitting the model
with(model.dat, barplot(by(attend, partyid, mean)))

# A much nicer barplot 
  # install.packages("RColorBrewer") 
library(RColorBrewer) 
?brewer.pal

  # first make color palette with a color for each level of partyid, with the
  # colors moving from deep blue to deep red to represent political spectrum
levs <- length(unique(model.dat$partyid)) # the number of unique values of partyid
display.brewer.pal(n = levs, "RdBu") # this is good but we want blue on the left and red on the right
my.palette <- brewer.pal(n = levs, "RdBu") # assign the color palette to my.palette
my.palette <- rev(my.palette) # reverse the order of the colors so blue is on the left
image(1:levs, 1, as.matrix(1:levs), col = my.palette, axes = F) # much better
  
  # make labels for the bars
bar.labs <- c("strong dem","dem","ind (dem)","ind","ind (rep)", "rep","strong rep")
  # make the barplot
with(model.dat, 
     barplot(by(attend, partyid, mean), 
             col = my.palette, names = bar.labs, 
             cex.names = 0.8, # make the names a bit smaller than the default
             las = 2, # rotate the bar labels to be perpendicular to the axis
             main = "Mean of attend by levels of partyid")) 


# Barplot with ggplot
gg.dat <- ddply(model.dat, "partyid", summarise, mean.attend = mean(attend))
g <- ggplot(gg.dat, aes(x = partyid, y = mean.attend, fill = factor(partyid))) 
colors_and_labels <- scale_fill_manual(values = my.palette, labels = bar.labs, name = "PartyID")
g + geom_bar(stat = "identity") + colors_and_labels





# Omitted variables test
resettest(lm.attend, pow = 2:4)

# Curvilinear regression
lm.attend2 <- lm(attend ~ partyid + I(partyid^2), data = sub, partyid < 7)
summary(lm.attend2)

# Plotting the curve
with(lm.attend2, curve(coefficients%*%rbind(1,x,x^2), 
                       xlab = "partyid", ylab = "",
                       xlim = c(0,6), ylim = c(3,5), 
                       lwd = 2, col = "darkblue"))





# Log transformations -----------------------------------------------------
# _________________________________________________________________________

# extract variable realrinc without any missing values (for demonstration
# purposes)
realrinc <- na.omit(GSS$realrinc)

# make plotting area have 2 rows and 1 column 
par(mfrow = c(2,1)) 
  # Note: often R-Studio's plot window doesn't handle these different layouts 
  # well. Click on magnifying glass labeled "zoom" to get a much better sense
  # for what the plot really looks like

# Compare distributions of realrinc and log(realrinc)
  # histogram of realrinc
truehist(realrinc, col = "maroon", yaxt = "n", cex.axis = 0.8) 
  # add normal density curve
curve(dnorm(x, mean = mean(realrinc), sd = sd(realrinc)), 
      lwd = 2, col = "skyblue", add=T)
  # histogram of log(realrinc)
truehist(log(realrinc), col = "maroon", yaxt = "n", cex.axis = 0.8)
  # add normal density curve
curve(dnorm(x, mean = mean(log(realrinc)), sd = sd(log(realrinc))), 
      lwd = 2, col = "skyblue", add=T)
# reset plotting layout
par(mfrow=c(1,1)) 



### Log-log model ###
lm.realrinc <- lm( I(log(realrinc)) ~ I(log(hrs1)) + as.factor(sex), 
                   data = GSS_2010, hrs1 > 0) # hrs1 > 0 b/c log(0) = -Inf  
summary(lm.realrinc)


### Level-log model ###
Tab(GSS_2010$wrkstat)
GSS_2010$working <- with(GSS_2010, wrkstat == 0 | wrkstat == 1)
Tab(GSS_2010$working)

summary(lm(wordsum ~ I(log(tvhours)) + working, data = GSS_2010, tvhours>0))

