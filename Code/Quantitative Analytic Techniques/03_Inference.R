#=====================================================================#
#====== CONFIDENCE INTERVALS & DIFFERENCES IN MEANS/PROPORTIONS ======#
#=====================================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014




# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# load packages
library(QMSS)
library(plyr)

# load data
load("GSS.RData")
load("GSS_2010.RData")



# Confidence Intervals ----------------------------------------------------
# _________________________________________________________________________


### Looking at internet hours ###

# Create a new variable "totalhr" adding "wwhr" and "emailhr" variables
GSS_2010$totalhr <- with(GSS_2010, wwwhr + emailhr)
summary(GSS_2010$totalhr) 

# Manually calculate 95% confidence interval:
mean <- mean(GSS_2010$totalhr, na.rm = T)
sd <- sd(GSS_2010$totalhr, na.rm = T)
n <- sum(!is.na(GSS_2010$totalhr))
ci <- mean + sd/sqrt(n) * qt(p = c(0.025, 0.975), df = n - 1) # qt() is the quantile function for the t distribution
ci

# Have R compute the 95% confidence interval
t.test(GSS_2010$totalhr)$conf.int

# Or the 99% confidence interval
t.test(GSS_2010$totalhr, conf.level = 0.99)$conf.int

# Compare confidence intervals for various confidence levels 
levs <- c(0.1, 0.5, 0.9, 0.95, 0.99, 0.9999) # the levels to use
ci <- mat.or.vec(length(levs), 2) # a matrix of 0s with length(levs) rows and 2 columns
  
  # Many ways to do this. This is one way to do it using a loop (over the indices of the levs vector)
for(j in 1:length(levs)){ 
    # set jth row of ci to be the confidence interval obtained using jth level in levs
    ci[j,] <- t.test(GSS_2010$totalhr, conf.level = levs[j])$conf.int
  }
rownames(ci) <- paste0(100*levs,"%") # make the row names the conf levels as percents (mutiple by 100 & add % symbol)
colnames(ci) <- c("Lower","Upper") # make the column names "lower" and "upper" for the bounds of the interval
round(ci, 3) # display the confidence intervals rounded to 3 decimal places

  # We could do a similar loop using the manual calculation of the CIs instead of the t.test function
ci <- mat.or.vec(length(levs), 2) 
for(j in 1:length(levs)){ 
  q <- (1 - levs[j])/2
  q <- c(q, 1 - q)
  ci[j,] <- mean + sd/sqrt(n)*qt(q, df = n-1)
}
rownames(ci) <- paste0(100*levs,"%")
colnames(ci) <- c("Lower","Upper") 
round(ci, 3) 


### Politicians example ###

# Use the simple Tab function we wrote to get a table of counts and percentages
# The function is also included in the QMSS package
?Tab
Tab(GSS_2010$polgreed)
Tab(GSS_2010$polgreed, useNA = "ifany")

# Create a new variable new.polgreed by recoding polgreed with mapvalues function in plyr package
GSS_2010$new.polgreed <- mapvalues(GSS_2010$polgreed,
                                   from = 1:5, # old values
                                   to = c(rep(1,2), rep(0,3))) # new values (note: rep(X,k) repeats X k times)
Tab(GSS_2010$new.polgreed)

# we can also assign the output from Tab() to an object
tab.polgreed <- Tab(GSS_2010$new.polgreed) 
tab.polgreed

# Get 95% confidence interval for proportion using binom.test() 
  # we pass to binom.test() a vector of "success" and "failure" counts 

  # first, we can get just the counts from tab.polgreed in several ways
tab.polgreed[, 1] 
tab.polgreed[, "Count"]
tab.polgreed[1:2]

  # this gives us the count of 0s and then the count of 1s, but for binom.test() we want to give
  # it the "successes" (the 1s) first so we can use tab.polgreed[2:1] instead of tab.polgreed[1:2]
tab.polgreed[2:1]
 
  # now use binom.test() 
binom.test(tab.polgreed[1:2])
binom.test(tab.polgreed[1:2])$conf.int # just get the confidence interval
  


# Test for difference in means and proportions ----------------------------
# _________________________________________________________________________


### Working hours and race example ###

# Make indicator variable for race = white
GSS_2010$white <- ifelse(GSS_2010$race == 1, 1, 0)
Tab(GSS_2010$white)

# Two-sample t-test (for difference of two means) with variances assumed to be equal
t.test(hrs1 ~ white, data = GSS_2010, var.equal = T) # set var.equal = F to not assume equal variances

### Gender and kindness example ###

# extract 2004 subset of cumulative GSS
GSS_2004 <- subset(GSS, year==2004) 

# look at levels of kindpers variable
Tab(GSS_2004$kindpers)

# create indicator variable "kind" for kindpers==1 
GSS_2004$kind <- ifelse(GSS_2004$kindpers == 1, 1, 0)

# Test for differences in proportions between genders with prop.test()
table.kind <- with(GSS_2004, table(sex, kind))
table.kind
table.kind[, 2:1] # flip the order of the two columns so that the column for kind=1 is first
prop.test(table.kind[, 2:1]) 

