#==============================================#
#====== AGE-PERIOD-COHORT (APC) ANALYSIS ======#
#==============================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# Set working directory
setwd("INSERT PATH TO DIRECTORY")

# load packages
library(QMSS)
library(ggplot2)
library(plyr)
library(rms)

# Load the cumulative GSS dataset 
load("GSS.RData")



# Making categories of age, period, cohort --------------------------------
# _________________________________________________________________________

vars <- c("natcrime", "age", "year", "cohort", "sex")
sub <- GSS[, vars]
sub$n.natcrime <- ReverseThis(sub$natcrime)
sub <- na.omit(sub)

# Break year, age and cohort each into 10 groups
  # the mutate function in plyr (like the transform function in base R adds new
  # variables to a data frame made from functions of existing variables)
sub <- mutate(sub,
              year.cut = cut(year, breaks = 10, labels = F, right = F),
              age.cut = cut(age, breaks = 10, labels = F, right = F),
              cohort.cut = cut(cohort, breaks = 10, labels = F, right = F))


# Tabulate age profiles for each cohort
# install.packages("Epi")
library(Epi)
tab_age.cohort <- stat.table(index = list("AGE" = age.cut, "COHORT" = cohort.cut),
                             contents = mean(n.natcrime), 
                             margins = TRUE,
                             data = sub)
print(tab_age.cohort, digits = 3)

# Graph age-profiles for each cohort
age.cohort <- ddply(sub, c("age.cut", "cohort.cut"), summarise, NATCRIME = mean(n.natcrime))
g_age.cohort <- ggplot(age.cohort, aes(x=age.cut, y=NATCRIME, 
                                       group=cohort.cut, color = factor(cohort.cut)))
g_age.cohort <- g_age.cohort + geom_point(size = 3) + geom_line() 
g_age.cohort + ylim(2.3,2.9) 

# Tabulate cohort nnatcrime scores over time
tab_year.cohort <- stat.table(index = list("YEAR" = year.cut, "COHORT" = cohort.cut),
                             contents = mean(n.natcrime), 
                             margins = TRUE,
                             data = sub)
print(tab_year.cohort, digits = 3)

# Graph age-profiles for each cohort
year.cohort <- ddply(sub, c("year.cut", "cohort.cut"), summarise, NATCRIME = mean(n.natcrime))
g_year.cohort <- ggplot(year.cohort, aes(x=year.cut, y=NATCRIME, 
                                         group=cohort.cut, color = factor(cohort.cut)))
g_year.cohort <- g_year.cohort + geom_point(size = 3) + geom_line()
g_year.cohort + ylim(2.3,2.9) 


# Tabulate age nnatcrime scores over year
tab_age.year <- stat.table(index = list("YEAR" = year.cut, "AGE" = age.cut),
                             contents = mean(n.natcrime), 
                             margins = TRUE,
                             data = sub)
print(tab_age.year, digits = 3)

# Graph age-profiles for each cohort
age.year <- ddply(sub, c("age.cut", "year.cut"), summarise, NATCRIME = mean(n.natcrime))
g_age.year <- ggplot(age.year, aes(x=age.cut, y=NATCRIME, 
                                   group=year.cut, color = factor(year.cut)))
g_age.year <- g_age.year + geom_point(size = 3) + geom_line()
g_age.year + ylim(2.3,2.9) 


# Overall trend
by.year <- ddply(sub, "year", summarise, NATCRIME = mean(n.natcrime))
g_year <- ggplot(by.year, aes(x = year, y = NATCRIME)) 
g_year <- g_year + geom_line(color = "navyblue", lwd = 1.5) + geom_point(color = "skyblue", size = 3)
g_year + ylim(2.3, 2.9)


# Perfect collinearity = failure (year is omitted)
summary(lm(n.natcrime ~ cohort + age + year, data = sub)) 

# Our first model 
lm.natcrime <- lm(n.natcrime ~ factor(age.cut) + factor(year.cut) + factor(cohort.cut), data = sub)
summary(lm.natcrime)

# Which years correspond to the year cuts?
ddply(sub, "year.cut", summarise, min = min(year), max = max(year))

# Could we reduce the whole model just to period effects? 
lm.natcrime2 <- lm(n.natcrime ~ factor(year.cut), data = sub)
summary(lm.natcrime2)

# Compare R^2
c(Model1 = summary(lm.natcrime)$adj, Model2 = summary(lm.natcrime2)$adj)


# Save our progress 
rm(GSS) # remove the GSS data set. It's big and we don't need to save the whole thing again
save.image(file = "APC.RData")
