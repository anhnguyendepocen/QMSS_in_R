# To do: 
# check recodes n.colhomo and n.fund 

#========================================================#
#====== LINEAR PROBABILITY, LOGIT, & PROBIT MODELS ======#
#========================================================#

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
library(visreg)

# Load the cumulative GSS and the GSS_2010 dataset 
load("GSS.RData")
load("GSS_2010.RData")




# Linear Probability Models -----------------------------------------------
# _________________________________________________________________________


### Have Muslims made a positive contribution to the country? ###
sub <- GSS[, c("contmslm", "relig", "polviews")]

Tab(sub$contmslm)
sub$muslim <- sub$contmslm == 1
Tab(sub$muslim)


Tab(sub$relig)
sub$relig.muslim <- sub$relig == 9
Tab(sub$relig.muslim)


# linear probability model (LPM)
lpm.muslim <- lm(muslim ~ relig.muslim, data = sub)
summary(lpm.muslim)

# LPM including polviews
Tab(sub$polviews)
lpm.muslim2 <- update(lpm.muslim, ~ . + polviews)
summary(lpm.muslim2)




### Among 13-17 olds, how many have had a romantic relationship? ###

# Load Stata .dta file using foreign package
library(foreign)
AdHealth <- read.dta("Data/adHealth.dta")
AdHealth <- with(AdHealth, # Take needed variables and give them meaningful names
                 data.frame(relationship = H1RR1, 
                            attractive = H1IR1,  
                            smoking = H1TO1,
                            birthYear = H1GI1Y,
                            momEduc = H1NM4,
                            noClubs = S44 ))

# Recodes
  # "have you had a romantic relationship?"
Tab(AdHealth$relationship) 
AdHealth$romance <- ifelse(AdHealth$relationship == "Yes", 1, 0)
Tab(AdHealth$romance)

  # interviewer's opinion on R's attractiveness
Tab(AdHealth$attractive)
AdHealth$attractive <- as.numeric(AdHealth$attractive)
AdHealth$attractive[AdHealth$attractive >= 6] <- NA
Tab(AdHealth$attractive)

  # ever smoke a cigarette
Tab(AdHealth$smoking)
levels(AdHealth$smoking)
levels(AdHealth$smoking) <- c("No", "Yes", NA, NA, NA)
Tab(AdHealth$smoking)
AdHealth$smoking <- ifelse(AdHealth$smoking == "Yes", 1, 0)
Tab(AdHealth$smoking)

  # mother's educ level
Tab(AdHealth$momEduc)
AdHealth$momEduc <- as.numeric(AdHealth$momEduc)
AdHealth$momEduc[AdHealth$momEduc >= 10] <- NA
Tab(AdHealth$momEduc)

  # doesn't participate in clubs/organizations, etc
Tab(AdHealth$noClubs) 
AdHealth$noClubs <- ifelse(AdHealth$noClubs == "Marked", 1, 0)
Tab(AdHealth$noClubs)

  # get (rough) age at time of interview (in 1995)
str(AdHealth$birthYear) # it's a factor variable
Tab(AdHealth$birthYear)
AdHealth$birthYear[AdHealth$birthYear == "Refused"] <- NA
Tab(as.numeric(AdHealth$birthYear))
AdHealth$age <- 95 - (as.numeric(AdHealth$birthYear) + 73)
Tab(AdHealth$age)



# take a subset to work with temporarily
vars <- c("romance", "attractive", "smoking")
sub <- na.omit(AdHealth[,vars])

# cross-tabulations
with(sub, table(attractive, romance))
with(sub, table(smoking, romance))

# linear probability model
lpm.romance <- lm(romance ~ attractive + smoking, data = sub)
summary(lpm.romance)

sub$yhat <- lpm.romance$fitted # or equivalently sub$yhat <- predic(lpm.romance)
sub$resids <- lpm.romance$residuals

# graphing it
with(sub, 
     plot(jitter(attractive), # jitter adds a tiny bit of noise so points very close together can be distinguished
          jitter(romance),
          col = "maroon", cex = 0.75))
abline(lm(yhat ~ attractive, data = sub), lwd = 2, col = "navyblue")


# Normality of errors assumption is violated 
truehist(sub$resids, # histogram of residuals 
         col = "skyblue", yaxt = "n", 
         cex.axis = 0.8, xlab = "residuals") 
curve(dnorm(x, mean = mean(sub$resids), sd = sd(sub$resids)), # add normal density curve
      lwd = 2, col = "orangered", add=T)

# Fitted values can be outside [0,1]
range(sub$yhat) # Here it's not a problem but it can be

# Homoskedasticity assumption violated
bptest(lpm.romance)   # or bptest(lpm.romance, studentize = F) to get same test statistic value as Stata's hettest

# Variance or residuals by categories of the X variables
ddply(sub, "attractive", summarize, var.resids = var(resids))
ddply(sub, "smoking", summarize, var.resids = var(resids))

  # or with base R function tapply()
with(sub, tapply(resids, attractive, var))
with(sub, tapply(resids, smoking, var))

  # or with base R function by()
with(sub, by(resids, attractive, var))
with(sub, by(resids, smoking, var))

  # or with base R function aggregate()
with(sub, aggregate(resids, by = list(attractive = attractive), var))
with(sub, aggregate(resids, by = list(smoking = smoking), var))





# Logit models ------------------------------------------------------------
# _________________________________________________________________________

# The logistic curve, 
curve(exp(x)/(1 + exp(x)), from = -4 ,to = 4, col = "seagreen", lwd = 2, 
      xlab = "logit(p)", ylab = "p", main = "The Logistic Curve")
  # or we can use plogis()
curve(plogis, from = -4 ,to = 4, col = "navyblue", lwd = 2, 
      xlab = "logit(p)", ylab = "p", main = "The Logistic Curve")


# Start with a proportion
mean(sub$romance)

# Logit model (use glm() with family = binomial)
logit.romance <- glm(romance ~ attractive + smoking, data = sub, family = binomial)
summary(logit.romance)

# Confidence intervals for the coefficients
confint(logit.romance)

# Odds ratios
exp(coef(logit.romance))

# Confidence intervals for the odds ratios
exp(confint(logit.romance))

# I've also combined these steps for getting the odds ratios and CIs in a function logitOR
?logitOR
logitOR(logit.romance) # gives 95% confidence intervals by default
logitOR(logit.romance, intercept = FALSE) # setting intercept = FALSE drops the intercept
logitOR(logit.romance, intercept = FALSE, level = 0.99) # get 99% CIs



# Predicted probabilities
  # manually
b <- logit.romance$coef # extract coefficients
x.vals <- c(1,1,0) # x-values for person with attractive = 1 & smoking = 0 (include the first 1 to be multipled by the intercept)
exp(x.vals%*%b)/(1 + exp(x.vals%*%b)) # predicted probability (we use the matrix multiplication %*% operator which, for two vectors like we have here, returns the dot product)
x.vals <- c(1,5,1) # x-values for a different person attractive = 5 & smoking = 1
exp(x.vals%*%b)/(1 + exp(x.vals%*%b)) # predicted probability

  # manually using plogis()
x.vals <- c(1,1,0)
plogis(x.vals%*%b)
x.vals <- c(1,5,1) 
plogis(x.vals%*%b)

  # using predict() function
predict(logit.romance, 
        type = "response", # type = "link" would give us predicted log-odds instead
        newdata = data.frame(  # enter the data (i.e. x values) we want to use for the predictions 
          smoking = c(0,1), # here we predict for the same two people as above, one with
          attractive = c(1,5)))            # smoking=0, attractive=1 and the other with values of 1 and 5


### Bigger model ###
sub.big <- na.omit(AdHealth)

# logit model
logit.romance2 <- glm(romance ~ attractive + smoking + momEduc + age + noClubs, 
                      data = sub.big, family = binomial)  
summary(logit.romance2)

# Predicted probabilities

  # predicted probability at each value of smoking with other vars fixed 
pred.dat <- with(sub.big, 
                 data.frame( # make data frame with the values we want
                   smoking = 0:1, 
                   attractive = median(attractive),
                   momEduc = median(momEduc),
                   age = median(age),
                   noClubs = 0 ))
preds1 <- cbind(pred.dat, # combine the data frame with predicted probabilities
                predicted.prob = predict(logit.romance2, type = "response", newdata = pred.dat))
print(preds1, digits = 3)

  # now let smoking AND age vary
pred.dat <- with(sub.big, 
                 expand.grid( # expand.grid() creates a data frame from all combinations of the supplied vectors
                   age = min(age):max(age),
                   smoking = 0:1,
                   attractive = median(attractive),
                   momEduc = median(momEduc),
                   noClubs = 0))
preds2 <- cbind(pred.dat, 
                predicted.prob = predict(logit.romance2, type = "response", newdata = pred.dat))
print(preds2, digits = 3)

  # predicted probability at each value of smoking at both the min and max values of attractive
pred.dat <- with(sub.big, expand.grid( 
  smoking = 0:1,
  attractive = range(attractive),
  momEduc = median(momEduc),
  age = median(age),
  noClubs = 0))
preds3 <- cbind(
  pred.dat, 
  predicted.prob = predict(logit.romance2, type = "response", newdata = pred.dat))
print(preds3, digits = 3)

  # predicted probs & 95% intervals for all combinations of attractive & smoking found in the data
pred.dat <- with(sub.big, expand.grid( 
  attractive = sort(unique(attractive)),
  smoking = 0:1, 
  momEduc = median(momEduc),
  age = median(age),
  noClubs = 0))
  
preds4 <- cbind(pred.dat, 
                predict(logit.romance2, 
                        type = "link", # if we want to compute CIs we first need to get the predictions on the log-odds scale
                        se = T, # and we need the standard errors of the predictions
                        newdata = pred.dat))
preds4 <- within(preds4, { # now we can convert them into probabilities and intervals around the predicted probabilities
  PredictedProb <- plogis(fit)
  lower.bound <- plogis(fit - 1.96*se.fit)
  upper.bound <- plogis(fit + 1.96* se.fit)
})
print(preds4, digits = 3)
preds4 <- preds4[,-c(6:8)]
print(preds4, digits = 3)


# I've also combined these steps for getting the predicted probabilities and confidence intervals
# in a function called predProb in the QMSS package
?predProb
predProb(logit.romance2, predData = pred.dat)
predProb(logit.romance2, predData = pred.dat, ci = F)
predProb(logit.romance2, predData = pred.dat, level = 0.99)



# compute changes in predicted probabilities as attractive goes from its min to max, 
# from its mean-1sd to mean+1sd, and from 0 to 1
  # fix everything at its mean except let attractive take on min, max, mean-sd, mean+sd, 0 and 1 
pred.dat <- with(sub.big, data.frame( 
  attractive = c(range(attractive), mean(attractive) + c(-1,1)*sd(attractive), 0, 1),
  smoking = mean(smoking), 
  momEduc = mean(momEduc),
  age = mean(age),
  noClubs = mean(noClubs)
  ))
preds5 <- predict(logit.romance2, type = "response", newdata = pred.dat) 

  # compute changes
preds5 <- rbind(
  "min -> max" = preds5[2] - preds5[1],  # change from min to max
  "mean-sd -> mean+sd" = preds5[4] - preds5[3], # change from 1 SD below the mean to 1 SD above 
  "0 -> 1" = preds5[6] - preds5[5]) # change from 0 to 1
colnames(preds5) <- "pr.change"                  
print(preds5, digits = 3)




# Probit models -----------------------------------------------------------
# _________________________________________________________________________

# Plot of standard normal CDF
curve(pnorm, -4, 4, col = "navyblue", lwd = 2, main = "CDF, Std Normal Dist")

# Plot of the inverse cdf 
curve(qnorm, 0, 1, col = "orangered", lwd = 2, main = "Inverse CDF, Std Normal Dist")

# Logit vs probit
curve(pnorm, -4, 4, col = "orangered", lwd = 2)
curve(plogis, -4, 4, col = "navyblue", lwd = 2, add = T)
legend("right", c("Probit", "Logit"), lwd = 2, col = c("orangered","navyblue"), bty="n")



# Probit model 
probit.romance <- glm(romance ~ attractive + smoking, 
                      data = sub, 
                      family = binomial(link=probit)) # set link = probit
summary(probit.romance)

# predicted probability for non-smoker with an attractiveness score of 1
p <- predict(probit.romance, type = "response", 
             newdata = data.frame(smoking = 0, attractive = 1))

# show this predicted probability as a point on the normal CDF curve
curve(pnorm, -4, 4, col = "navyblue", lwd = 2, lty = 2, ylab = "Probability")
points(qnorm(p), p, col = "maroon", pch = 19) 
text(qnorm(p), p, pos = 4, # label the point with its coordinates
     labels = paste0("(",round(qnorm(p),2), ", ", round(p,2), ")" ),
     col="maroon")


# the QMSS predProb() function also works with probit models (it will detect if the link function is logit or probit and make appropriate adjustments)
predProb(probit.romance, predData = data.frame(smoking = 0, attractive = 1))


# Comparing the coefficients from the logit and probit models
cbind(Logit = logit.romance$coef, Probit = probit.romance$coef)






# Logit with interactions -------------------------------------------------
# _________________________________________________________________________

### Would you let a homosexual teach college? ###

# recodes
Tab(GSS_2010$colhomo, useNA = "ifany")
GSS_2010$n.colhomo <- ifelse(GSS_2010$colhomo == 5, 1, 0)
Tab(GSS_2010$n.colhomo)

Tab(GSS_2010$fund)
GSS_2010$n.fund <- factor(GSS_2010$fund, labels=c("fundamentalist","moderate","liberal"))
Tab(GSS_2010$n.fund, useNA = "ifany")

table(GSS_2010$n.colhomo, GSS_2010$n.fund)

# Logistic regression
logit.colhomo <- glm(n.colhomo ~ as.numeric(n.fund) + educ + age, data = GSS_2010, family = binomial)
summary(logit.colhomo)

# With interaction
logit.colhomo2 <- glm(n.colhomo ~ n.fund*educ + age, data = GSS_2010, family = binomial)
summary(logit.colhomo2)

# Plot predicted logits and confidence intervals by level of n.fund
visreg(logit.colhomo2, "educ", by = "n.fund", 
       partial = F, overlay = T, 
       xlab = "Highest year of school completed", 
       ylab = "Predicted logit")

# Plot predicted probabilities by level of n.fund
#   same as plotting predicted logits but we add 'scale = "response"' 
visreg(logit.colhomo2, "educ", by = "n.fund", 
       partial = F, overlay = T, 
       xlab = "Highest year of school completed", 
       ylab = "Predicted probability", 
       scale= "response")



# Plot predicted probabilities by level of n.fund for different values of age
plot.by.age <- function(AGE){
  visreg(logit.colhomo2, "educ", by = "n.fund", 
         overlay = T, partial = F, band = F, legend = F,
         xlab = "", ylab = "", 
         line = list(col = c("navyblue", "maroon3", "darkcyan")), 
         cond = list(age = AGE), print.cond = T,
         scale = "response")
  legend("top", legend = paste("Age =",AGE), bty = "n", cex = 1.25)
}

par(mfrow = c(1,3), oma = c(1.5,1.5,1.5,0))
for(AGE in c(20, 50, 80)){  # we'll loop over the different age values (here we use 20, 50 and 80)
  plot.by.age(AGE) # use the newly created function
}
# instead of the loop we could also have used sapply(X = c(20,50,80), FUN = plot.by.age)

mtext(text = paste("----", levels(GSS_2010$n.fund)), col = c("navyblue","maroon3", "darkcyan"),
      at = c(0.25, .55, 0.8), outer = T, line = -1)
mtext(text = "Predicted probability", side = 2, outer = T)
mtext(text = "Highest year of school completed", side = 1, outer = T, line = -1)
