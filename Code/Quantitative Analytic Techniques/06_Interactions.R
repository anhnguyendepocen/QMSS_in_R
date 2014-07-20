#==========================#
#====== INTERACTIONS ======#
#==========================#

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
library(plyr)

# Load the GSS cumulative dataset & 2010 subset
load("GSS.RData")
load("GSS_2010.RData")





# Predict income from education and gender --------------------------------
# _________________________________________________________________________

vars <- c("realrinc", "educ", "sex")
sub <- GSS[, vars]

sub$female <- sub$sex == 2
lm.income <- lm(realrinc ~ educ + female, data = sub)
summary(lm.income)


# Plot of education vs predicted income comparing men and women 
  # There are many many ways to do this. Here are several:
  
  # Using visreg package
# install.packages("visreg")
library(visreg)
visreg(lm.income, # the model
       xvar = "educ", # variable to plot on x-axis
       xlab = "highest year of school completed", # title for x-axis
       ylab = "Income", # title for y-axis
       by = "female", # plot one line for males and one for females
       overlay = T, # plot the lines for females and males on the same plot instead of two separate ones
       partial = F, # don't plot residuals 
       band = F, # don't plot confidence interval bands,
       legend = F, # don't print legend (we'll add our own)
       cex.axis = 0.7, # shrink the axis annotations (the tick mark labels) a bit
       bty = "l", # draw an L-shaped box around the plot (instead of default which is a full rectangle)
       line = list(col = c("darkcyan", "lightcoral"))) # colors for the lines  
legend("top", c("Males", "Females"), # add a legend to the plot
       lwd = 2, col = c("darkcyan", "lightcoral"), bty = "n")

  # Or using interaction.plot() function
interaction.plot(lm.income$model$educ, lm.income$model$female, lm.income$fitted,
                 col =  c("darkcyan", "lightcoral"), # colors for the lines
                 lty = 1, lwd = 2,
                 legend = F, # don't print a legend (we'll add our own after b/c the default legends are very ugly)
                 xlab = "Highest year of school completed", # captions for the axes
                 ylab = "Income")
legend("bottomright", c("Males", "Females"), col = c("darkcyan", "lightcoral"), lwd = 2)

  # Or for practice we can do the plot manually using the plot() and points() functions
model.dat <- cbind(lm.income$model, xb = lm.income$fitted)
with(model.dat,{
  # plot for females 
  plot(educ[female==T], xb[female==T], 
       type = "l", # type = "l" for lines
       col = "lightcoral", # color of the line for females
       xlab = "Highest year of school completed", 
       ylab = "Predicted Income")
  # add points & line for males 
  points(educ[female==F], xb[female==F], type="l", col="darkcyan")
  # add a legend to the plot 
  legend("bottomright", c("Males", "Females"), col = c("darkcyan", "lightcoral"), lwd = 2)
})


# Simple regression for men and women separately
lm.males <- lm(realrinc ~ educ, data = sub, female == 0)
summary(lm.males)
lm.females <- lm(realrinc ~ educ, data = sub, female==1)
summary(lm.females)

# Interaction model
lm.income2 <- lm(realrinc ~ educ + female + educ:female, data = sub)
summary(lm.income2)
  # we can also specify the same interaction model using the * operator like this
summary(lm(realrinc ~ educ*female, data = sub)) 

# Plot again, this time with interaction. 
  # Again, there are many ways to do this. Here are several:

  # Using visreg()
visreg(lm.income2, "educ", by = "female", overlay = T, partial = F, band = F, legend = F, 
       line = list(col = c("darkcyan", "lightcoral"))) 
legend("bottomright", c("Males", "Females"), lwd = 2, col = c("darkcyan", "lightcoral"), cex = 0.7)
  
  # Or using xyplot() function from lattice package
library(lattice)
xyplot(realrinc ~ educ, groups = female, data = sub, 
       type = "r", grid = T, lwd = 3, ylim=c(-5*10^4,10^5),
       auto.key = list(text = c("Males", "Females"), points = F, lines = T))

  # Or with ggplot
ggplot(sub, aes(x = educ, y = realrinc, group = female, colour = female)) + 
  geom_smooth(method = "lm", fullrange=T, se = F)

  # Or manually adding each piece of the plot one at a time
plot(lm.males$model$educ, lm.males$fitted, type = "n", xlab = "Predicted Income")
abline(lm.males, col = "darkcyan", lwd = 2)
abline(lm.females, col = "lightcoral", lwd = 2)
legend("top", c("Males", "Females"), col = c("darkcyan", "lightcoral"), lwd = 2, ncol = 2, bty = "n")




# Predicting educational attainment with number of siblings ---------------
# _________________________________________________________________________

vars <- c("educ", "sibs", "madeg", "family16", "age")
sub <- GSS[, vars]
sub$maBA <- sub$madeg %in% c(3,4)
Tab(sub$maBA)

# A simple multiple regression 
summary(lm(educ ~ sibs + maBA, data = sub))

# A simple regression for kids of <BA moms
summary(lm(educ ~ sibs, data = sub, maBA == 0))

# A simple regression for kids of +BA moms
summary(lm(educ ~ sibs, data = sub, maBA == 1))

# Interaction model
summary(lm(educ ~ sibs + maBA + sibs:maBA, data = sub))

# Plot: kids of BA+ moms vs. not, with interaction
visreg(lm(educ ~ sibs + maBA + sibs:maBA, data = sub),
       xvar = "sibs", by = "maBA", overlay=T, partial = F, band = F, legend = F, 
       line = list(col = c("cyan3", "purple3"))) 
legend("bottomleft", c("<BA", "BA+"), lwd = 2, col = c("cyan3", "purple3"), cex = 0.8)


# Add more variables to the model
sub$twobio <- sub$family16 == 1
Tab(sub$twobio)
lm.maBA <- lm(educ ~ sibs*maBA + age + twobio, data = sub)
summary(lm.maBA)

# Redo the interaction plot 
visreg(lm.maBA, "sibs", by = "maBA", overlay=T, band = F, partial = F, 
       line = list(col = c("cyan3", "purple3")), bty = "l", legend = F,
        # cond = list(), # conditional values of explanatory vars (default is median for numeric & most common category for factors)
       print.cond=TRUE) # print message telling us the conditional values of explanatory variables  
legend("topright", c("<BA", "BA+"), bty = "n", lwd = 2, col = c("cyan3", "purple3"), cex = 0.8)

  # Redo interaction plot fixing age 20
visreg(lm.maBA, "sibs", by = "maBA", overlay=T, band = F, partial = F,
       line = list(col = c("cyan3", "purple3")), bty = "l",
       cond = list(age = 20),
       print.cond=TRUE) 





# Does education alter fundamentalists opinion on evolution? --------------
# _________________________________________________________________________

vars <- c("educ", "fund", "evolved", "family16", "age")
sub <- GSS[, vars]
sub$fundamentalist <- sub$fund == 1
sub$evolution <- sub$evolved == 1

# Simple multiple regression
summary(lm(evolution ~ fundamentalist + educ, data = sub))

# Interaction model
summary(lm(evolution ~ educ*fundamentalist, data = sub))

# Plot (fundamentalist vs. not fundamentalist, with interaction)
visreg(lm(evolution ~ educ*fundamentalist, data = sub), 
       "educ", by = "fundamentalist", overlay=T, band = F, partial = F, 
       line = list(col = c("dodgerblue","orangered")), bty = "l", legend = F) 
legend("top", c("Not Fundamentalist", "Fundamentalist"), 
       bty = "n", lwd = 2, col = c("dodgerblue","orangered"), cex = 0.8)



### Centering the data at the mean ###

# Center the education variable & rerun regression with interaction
sub$center.educ <- scale(sub$educ, center = T, scale = F) # if we set scale = T then R will also divide by the standard deviation
summary(sub$center.educ) # mean should be 0
summary(lm(evolution ~ center.educ*fundamentalist, data = sub))




# Diminishing (or redundant) effects --------------------------------------
# _________________________________________________________________________

vars <- c("marital", "wordsum", "educ", "speduc")
sub <- GSS[, vars]
sub$married <- sub$marital == 1

# Without interaction
lm.wordsum <- lm(wordsum ~ married + educ, data = sub)
summary(lm.wordsum)

# With interaction
lm.wordsum2 <- lm(wordsum ~ married*educ, data = sub)
summary(lm.wordsum2)

# Graphing this relationship
visreg(lm.wordsum2, "educ", by = "married", overlay=T, band = F, partial = F, 
       line = list(col = c("darkgreen", "darkorchid")), bty = "l", legend = F) 
legend("top", c("Unmarried", "Married"), horiz = T, 
       bty = "n", lwd = 2, col = c("darkgreen", "darkorchid"), cex = 0.8)





# Continuous by continuous interactions -----------------------------------
# _________________________________________________________________________


### Wordsum, by educ & spouse’s educ ###
sub <- GSS[, c("wordsum", "educ", "speduc")]

# Without interaction
summary(lm(wordsum ~ educ + speduc, data = sub))

# With interaction
lm.speduc <- lm(wordsum ~ educ*speduc, data = sub)
summary(lm.speduc)


# Plotting the relationship, fixing speduc at 0, mean-sd, mean+sd, and 20
# first get mean and sd of speduc
m <- with(lm.speduc$model, mean(speduc)); sd <- with(lm.speduc$model, sd(speduc))
# now use visreg() using our desired values as the values of the "breaks" argument 
visreg(lm.speduc, "educ", by = "speduc", breaks = c(0,m-sd,m+sd,20), 
       line = list(col = c("orangered", "dodgerblue4", "purple4", "turquoise4")),
       overlay=T, band = F, partial = F, bty = "l", legend = F) 
# add a legend (we can do it in one step or we can define the legend text first and use it after with the legend() command)
# below, bquote() allows us to combine mathematical symbols and numeric variables in our legend (see ?bquote and ?plotmath for more on this)
legend.text <- c(0, 
                 bquote(bar(x) - sd == .(round(m - sd, 2)) ), 
                 bquote(bar(x) + sd == .(round(m + sd, 2)) ),
                 20)
legend("bottomright", legend = as.expression(legend.text),
       col = c("orangered", "dodgerblue4", "purple4", "turquoise4"), 
       lwd = 2, bty = "n", title.adj=0, cex = 0.7, title = "Spouse's Education")




### Predicting health ###
sub <- GSS[, c("health", "educ", "attend", "age")]

# Reverse order of health variable
Tab(sub$health) 
sub$r.health <- ReverseThis(sub$health)
Tab(sub$r.health)

# Basic multiple regression 
summary(lm(r.health ~ educ + attend + age, data = sub))

# Interaction model
lm.health <- lm(r.health ~ educ*attend + age, data = sub)
summary(lm.health)

# Plotting the relationship
visreg(lm.health, "educ", by = "attend", breaks = c(0,4,8), overlay=T, band = F, partial = F,
       line = list(col = c("darkgreen", "darkorchid","royalblue")), bty = "l", legend = F) 
legend("bottomright", paste("Attend = ", c(0,4,8)), bty = "n", lwd = 2, 
       col = c("darkgreen", "darkorchid","royalblue"), cex = 0.8)


