#==========================================================#
#====== MULTILEVEL MODELS, CORRELATED RANDOM EFFECTS ======#
#==========================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014


# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# load packages
library(QMSS)
library(memisc)
library(plyr)
library(psych)
library(visreg)

# Load the cumulative GSS dataset 
load("GSS.RData")





# Multilevel Models -------------------------------------------------------
# _________________________________________________________________________

### Does race influence how emotionally close you feel to African Americans? ###

vars <- c("sampcode", "year", "closeblk", "race", "age")
sub <- GSS[,vars]

# create community IDs
sub$sampyear <- with(sub, sampcode*10^4 + year)
head(Tab(sub$sampyear), 15)

# Dependent variable: Closeblk = “In general, how close do you feel to blacks,” ranging from not at all close (1) to very close (9)
Tab(sub$closeblk)

# make community-level variables
sub <- ddply(sub, "sampyear", mutate, pct.black = 100*mean(race==2)) # percent black
with(sub, by(pct.black, race, summary)) # summary of pct.black by race

sub <- ddply(sub, "sampyear", mutate, 
             N = length(sampyear),  # number of obs per community
             n = seq_along(N))      # index each obs within communities


# naive OLS regression (with clustered se)
sub <- sub[-which(sub$race == 3), ] # only want to compare blacks vs whites
sub$white <- sub$race == 1
sub <- na.omit(sub)

lm.closeblk <- plm(closeblk ~ white + pct.black + age + year, 
                   data = sub, index = "sampyear", model = "pooling")
clusterSE(fit = lm.closeblk, cluster.var = "sampyear")


stdCoef(lm.closeblk) # check standardized coefficients



# install.packages("lme4")
library(lme4)

# Start with “variance components” model (the “empty” random intercept model)
nullmodel <- lmer(closeblk ~ (1 | sampyear), data = sub, REML = FALSE)
summary(nullmodel) 

# Note that in the output from summary(nullmodel) the Std. Dev. column in the Random Effects section
#   contains what STATA refers to as sigma_u and sigma_e. STATA also reports rho (fraction of variance due to u_i)
#   but this is not given in the R output. We'll want to compute it for the models below, so we can write
#   a function to avoid retyping the same commands multiple times. 

rho <- function(fit){
  varcor <- VarCorr(fit) # extract the variance components using VarCorr()
  varcor <- as.data.frame(varcor)[, "sdcor"] # get just the std devs we want
  sigma_u <- varcor[1] # get sigma_u
  sigma_e <- varcor[2] # get sigma_e
  rho <- sigma_u^2 / (sigma_u^2 + sigma_e^2) # compute rho (fraction of variance due to u_i)
  rho
}

# For future use the rho function is included in QMSS package
?rho

# Use our function to get the fraction of variance due to u_i
rho(nullmodel)


### Random intercept model ###
lmer.closeblk1 <- lmer(closeblk ~ white + pct.black + age + year + (1 | sampyear), 
                       data = sub, REML = FALSE)
summary(lmer.closeblk1)
rho(lmer.closeblk1)

# Make year relative to 1972
sub$n.year <- sub$year-1972
lmer.closeblk1 <- lmer(closeblk ~ white + pct.black + age + n.year + (1 | sampyear), 
                       data = sub, REML = FALSE)
summary(lmer.closeblk1)
rho(lmer.closeblk1)

# Run a separate regression for each sampyear, storing intercept, coefficient on white, and 
# the average of the fitted values.
  # get all unique values of sampyear
samps <- unique(sub$sampyear) 
regs <- sapply(
  X = 1:length(samps), # apply the function FUN (below) to each i in 1:length(samps)
  FUN = function(i){
    # regression for sampyear i
    lm <- lm(closeblk ~ white + age, data = sub, sampyear == samps[i])
    # return intercept, coeff on white, mean of fitted values 
    c(coef(lm)[1:2], mean(lm$fitted)) 
})

rownames(regs) = c("b_const", "b_white", "mean.fitted")

# get the averages over all values of sampyears
overall <- round(rowMeans(regs, na.rm = T), 2) 
overall

# Now plot coef on white vs. mean fitted for each sampyear
plot(x = regs["b_white",], y = regs["mean.fitted",], 
     pch = 20, col = rainbow(length(samps)))
points(x = overall[2], y = overall[3], 
       cex = 3, pch = 21, col = "black", bg = "navyblue")
leg.text <- paste("Overall Avg. = (", overall[2],",",overall[3],")")
legend("bottomright", bty = "n", legend = leg.text, 
       pch = 21, col = "black", pt.bg = "navyblue", pt.cex = 1.5)

# Now plot coef on white vs. intercept for each sampyear
plot(x = regs["b_white",], y = regs["b_const",], 
     pch = 20, col = rainbow(length(samps)))
abline(line(regs["b_white",], regs["b_const",]), 
       lty = 2, lwd = 3)
  # Zoom in a bit on the area with the highest density of points
plot(x = regs["b_white",], y = regs["b_const",], 
     pch = 20, col = rainbow(length(samps)),
     xlim = c(-5,5), ylim = c(0,15))
abline(line(regs["b_white",], regs["b_const",]), 
       lty = 2, lwd = 3)



### Random slopes too ### (now let coefficient on white vary by sampyear)
lmer.closeblk2 <- lmer(closeblk ~ white + pct.black + age + n.year + (1 + white | sampyear), 
                       data = sub, REML = FALSE)
summary(lmer.closeblk2)


### Are random slopes necessary? ###

# Likelihood ratio test via anova()
anova(lmer.closeblk1, lmer.closeblk2) # look at Chisq stat and p-value

# Or we can do the likelihood ratio test manually to get a sense for how the computation is done
  # the Chisq statistic is computed as (-2)*loglikelihood(model1) + 2*loglikelihood(model2)
  # or equivalently as deviance(model1) - deviance(model2)
chisq <- deviance(lmer.closeblk1) - deviance(lmer.closeblk2)
chisq # should be the same (up to rouding) as Chisq from anova(lmer.closeblk1, lmer.closeblk2)
df <- df.residual(lmer.closeblk1) - df.residual(lmer.closeblk2)
df # should be the same as Df from anova(lmer.closeblk1, lmer.closeblk2)
pval <- pchisq(chisq, df, lower.tail = FALSE)
pval # should be the same (up to rounding) as Pr(>Chisq) from anova(lmer.closeblk1, lmer.closeblk2)



### ￼Adding cross-level interactions ###
lmer.closeblk3 <- lmer(closeblk ~ white*pct.black + age + n.year + (1 + white | sampyear), 
                       data = sub, REML = FALSE)
summary(lmer.closeblk3)

# visualize the different slopes 
plotdata <- data.frame(fitted = fitted(lmer.closeblk3), 
                       pct.black = sub$pct.black, 
                       white = sub$white)

with(plotdata,{
  plot(NULL, xlim = c(0,100), ylim = c(4,10), xlab = "pct.black", ylab = "")
  abline(line(pct.black[white], fitted[white]), col = "orangered", lwd = 2)
  abline(line(pct.black[!white], fitted[!white]), col = "purple4", lwd = 2)
})
legend("top", legend = paste("Race = ", c("white", "black")), 
       bty = "n", lwd = 1, col = c("orangered", "purple4"))





# Correlated Random Effects -----------------------------------------------
# _________________________________________________________________________

# Load the GSS panel data
load("GSS_panel.RData")

vars <- c("satfin", "realinc", "race", "sex", "panelwave", "idnum")
sub <- pd[, vars]
sub$n.satfin <- ReverseThis(sub$satfin)

### Naive OLS ###
summary(lm(n.satfin ~ I(log(realinc)) + as.factor(panelwave), data = sub))

### With Fixed Effects ###
summary(
  plm(n.satfin ~ I(log(realinc)) + panelwave, data = sub,
            index = c("idnum", "panelwave"), model = "within"))

### Correlated Random Effects ###
  # get mean of log(realinc) by idnum
sub <- ddply(sub, "idnum", mutate, mean.lnrealinc = mean(log(realinc), na.rm = T))
summary(
  plm(n.satfin ~ I(log(realinc)) + mean.lnrealinc + panelwave, data = sub,
            index = c("idnum", "panelwave"), model = "random"))

  # add variable to sub containing max of race by idnum
sub <- ddply(sub, "idnum", mutate, max.race = max(race, na.rm = T))
summary(
  plm(n.satfin ~ I(log(realinc)) + mean.lnrealinc + as.factor(max.race) 
      + as.factor(sex) + panelwave, data = sub, 
      index = c("idnum", "panelwave"), model = "random"))

  # cross-level interactions, random slope & intercept
lmer.satfin <- lmer(n.satfin ~ I(log(realinc))*mean.lnrealinc + as.factor(max.race) + as.factor(sex) 
                    + as.factor(panelwave) + (1 + I(log(realinc)) | idnum), data = sub)
summary(lmer.satfin)


