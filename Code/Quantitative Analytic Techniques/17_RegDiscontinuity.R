#======================================#
#====== REGRESSION DISCONTINUITY ======#
#======================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3




# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# load packages
library(QMSS)
library(ggplot2)
library(plyr)


# Load the cumulative GSS dataset 
load("GSS.RData")




# Does turning 65 lead to increase in utilization of health services ------
# _________________________________________________________________________

sub <- GSS[, c("godoc", "age", "year")]
sub <- na.omit(sub)

Tab(sub$godoc)
sub$n.godoc <- mapvalues(sub$godoc, from = 1:2, to = 1:0)
with(sub, table(godoc, n.godoc))


# plot mean of age vs mean(n.godoc)
by.age <- ddply(sub, "age", summarize, mean.godoc = mean(n.godoc))
g_by.age <- ggplot(by.age, aes(x = age, y = mean.godoc)) + geom_line(color = "red3")
g_by.age
  # add vertical line at age = 65
g_by.age + geom_vline(xintercept = 65, lty = 2)
  # label the line
label_65 <- annotate("text", x = 63, y = 0.15, label = "Age = 65", angle = 90)
g_by.age + geom_vline(xintercept = 65, lty = 2) + label_65


# make the slopes for younger than 65 and older than 65
sub$ageY <- ifelse(sub$age >= 65, 0, sub$age - 65)
sub$ageO <- ifelse(sub$age < 65, 0, sub$age - 65)

# make the intercepts for for younger than 65 and older than 65
sub$intY <- ifelse(sub$age >= 65, 0, 1)
sub$intO <- ifelse(sub$age < 65, 0, 1)

# regression model 
lm.godoc <- lm(n.godoc ~ 0 + intY + intO + ageY + ageO, data = sub) # the 0 tells R not to add an intercept (we're using the intercept we created above)
summary(lm.godoc)
sub$yhat <- predict(lm.godoc) # get fitted values

# look at fitted values by age
ddply(sub, "age", summarize, yhat = mean(yhat), freq = length(age))

# plot the discontinuity
no_legend <- theme(legend.position = "none")
g_disc <- ggplot(sub, aes(x = age, y = yhat, group = intY, color = factor(intY))) + no_legend
g_disc + geom_line(size = 1.25) + geom_vline(xintercept = 65, lty = 2) + label_65




# ￼Another way to model this
sub$spike65 <- ifelse(sub$age == 65, 1, 0)
lm.godoc2 <- lm(n.godoc ~ age + spike65, data = sub)
summary(lm.godoc2)


# Include 2 years before and after 65
  # create indicator each age between 63 and 67 
for(i in 63:67){
  name <- paste0("age",i)
  sub <- within(sub, assign(name, age == i))
}

lm.godoc3 <- lm(n.godoc ~ age + age63 + age64 + age65 + age66 + age67, data = sub)
summary(lm.godoc3)

# test hypothesis of no difference between coefficients on age66TRUE and age65TRUE
library(car)
linearHypothesis(lm.godoc3, "age66TRUE - age65TRUE = 0") 


# RDestimate() from rdd package
# install.packages("rdd")
library(rdd)

rd.godoc <- RDestimate(n.godoc ~ age, data = sub, cutpoint = 65)
summary(rd.godoc)
plot(rd.godoc)

# or use RDplot function in QMSS package
?RDplot
RDplot(rd.godoc, col = c("blue", "green"), pts = T, xlab = "Age") 


# logit
logit.godoc <- glm(n.godoc ~ age + age63 + age64 + age65 + age66 + age67, 
                   data = sub, family = binomial)
summary(logit.godoc)
