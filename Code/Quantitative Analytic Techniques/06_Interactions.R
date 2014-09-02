#==========================#
#====== INTERACTIONS ======#
#==========================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

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
sub <- na.omit(sub)

sub$female <- sub$sex == 2
lm.income <- lm(realrinc ~ educ + female, data = sub)
summary(lm.income)

sub$fitted <- predict(lm.income)


# Plot of education vs predicted income comparing men and women 
g_realrinc <- ggplot(sub, aes(x = educ, y = fitted, group = female, color = female))
g_realrinc + stat_smooth(method = "lm", se = F)



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
  # With ggplot
g_realrinc.interaction <- ggplot(sub, aes(x = educ, y = realrinc, 
                                          group = female, color = female))
g_realrinc.interaction + stat_smooth(method = "lm", se = F)

  # or using visreg package
#install.packages("visreg")
library(visreg)
visreg(lm.income2, "educ", by = "female", overlay = T, partial = F, band = F, legend = F, 
       line = list(col = c("skyblue", "maroon"))) 
legend("bottomright", c("Males", "Females"), lwd = 2, col =  c("skyblue", "maroon"), cex = 0.7)
  
  # or using xyplot() function from lattice package
library(lattice)
xyplot(realrinc ~ educ, groups = female, data = sub, 
       type = "r", grid = T, lwd = 3, ylim=c(-5*10^4,10^5),
       auto.key = list(text = c("Males", "Females"), points = F, lines = T))

  # or manually adding each piece of the plot one at a time
plot(lm.males$model$educ, lm.males$fitted, type = "n", xlab = "educ", ylab = "fitted values")
abline(lm.males, col = "skyblue", lwd = 2)
abline(lm.females, col = "maroon", lwd = 2)
legend("top", c("Males", "Females"), col = c("skyblue", "maroon"), 
       lwd = 2, ncol = 2, bty = "n")




# Predicting educational attainment with number of siblings ---------------
# _________________________________________________________________________

vars <- c("educ", "sibs", "madeg", "family16", "age")
sub <- GSS[, vars]
sub$maBA <- sub$madeg %in% c(3,4)

# A simple multiple regression 
summary(lm(educ ~ sibs + maBA, data = sub))

# A simple regression for kids of <BA moms
summary(lm(educ ~ sibs, data = sub, maBA == 0))

# A simple regression for kids of +BA moms
summary(lm(educ ~ sibs, data = sub, maBA == 1))

# Interaction model
summary(lm(educ ~ sibs + maBA + sibs:maBA, data = sub))

# Plot: kids of BA+ moms vs. not, with interaction
gg_educ_interaction <- ggplot(sub, aes(x = sibs, y = educ, group = maBA, color = maBA))
gg_educ_interaction + stat_smooth(method = "lm", se = F, fullrange = T)

# Or using visreg
visreg(lm(educ ~ sibs + maBA + sibs:maBA, data = sub),
       xvar = "sibs", by = "maBA", overlay=T, partial = F, band = F, legend = F, 
       line = list(col = c("cyan3", "purple3"))) 
legend("bottomleft", c("<BA", "BA+"), lwd = 2, col = c("cyan3", "purple3"), cex = 0.8)


# Add more variables to the model
sub$twobio <- sub$family16 == 1
Tab(sub$twobio)
lm.maBA <- lm(educ ~ sibs*maBA + age + twobio, data = sub)
summary(lm.maBA)

# Make the interaction plot at both levels of twobio
panel_labels <- function(var,value) paste(var,"=",value) 
gg_educ_interaction2 <- ggplot(na.omit(sub), aes(x = sibs, y = educ, group = maBA, color = maBA))
gg_educ_interaction2 <- gg_educ_interaction2 + facet_grid(.~ twobio, labeller = panel_labels)
gg_educ_interaction2 + stat_smooth(method = "lm", se = F, fullrange = T) 



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
g_evolution <- ggplot(na.omit(sub), aes(x = educ, y = evolution, 
                                        group = fundamentalist, color = fundamentalist)) 
g_evolution + stat_smooth(method = "lm", se = F) + scale_color_manual(values = c("blue", "red"))

visreg(lm(evolution ~ educ*fundamentalist, data = sub), 
       "educ", by = "fundamentalist", overlay=T, band = F, partial = F, 
       line = list(col = c("dodgerblue","orangered")), bty = "l", legend = F) 
legend("top", c("Not Fundamentalist", "Fundamentalist"), 
       bty = "n", lwd = 2, col = c("dodgerblue","orangered"), cex = 0.8)



### Centering the data at the mean ###

# Center the education variable & rerun regression with interaction
sub$center.educ <- scale(sub$educ, center = T, scale = F) # setting scale = T will also divide by the standard deviation
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
g_wordsum <- ggplot(sub[!is.na(sub$married), ], aes(x = educ, y = wordsum, 
                                                    group = married, color = married))
line_colors <- scale_color_manual(values = c("darkgreen", "darkorchid"))
g_wordsum + stat_smooth(method = "lm", se = F) + line_colors

# with visreg
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
m <- round(mean(sub$speduc, na.rm = T), 2)
sd <- round(sd(sub$speduc, na.rm = T), 2)
plot.dat <- with(sub, expand.grid(educ = na.omit(unique(educ)), 
                                  wordsum = na.omit(unique(wordsum)), 
                                  speduc = c(0, m - sd, m + sd, 20)))
plot.dat <- data.frame(plot.dat, wordsum_pred = predict(lm.speduc, newdata = plot.dat))
g_speduc_fixed <- ggplot(plot.dat, aes(x = educ, y = wordsum_pred, 
                                       group = speduc, color = factor(speduc)))
g_speduc_fixed + stat_smooth(method = "lm", se = F) 

# customize the colors and legend text
  # bquote() allows us to combine mathematical symbols and numeric variables in
  # our legend (see ?bquote and ?plotmath for more on this)
legend.text <- c(0, 
                 bquote(bar(x) - sd == .(round(m - sd, 2)) ), 
                 bquote(bar(x) + sd == .(round(m + sd, 2)) ),
                 20)
colors_and_legend <- scale_color_brewer(palette = "Set1", labels = legend.text, name = "speduc")
g_speduc_fixed + stat_smooth(method = "lm", se = F) + colors_and_legend




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

# Plotting the relationship (using visreg for practice)
visreg(lm.health, "educ", by = "attend", breaks = c(0,4,8), 
       overlay=T, band = F, partial = F, bty = "l", legend = F,
       line = list(col = c("darkgreen", "darkorchid","royalblue"))) 
legend("bottomright", paste("Attend = ", c(0,4,8)), bty = "n", lwd = 2, 
       col = c("darkgreen", "darkorchid","royalblue"), cex = 0.8)


