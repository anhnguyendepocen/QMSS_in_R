#========================================================#
#====== RECODING & LOGICAL/CONDITIONAL EXPRESSIONS ======#
#========================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# load packages
library(plyr)

# load the previously saved 2010 subset of the GSS
load("GSS_2010.RData")




# Recoding variables ------------------------------------------------------
# _________________________________________________________________________


### Note: There are many many many many many ways to recode in R. Below are just a few of them.

# First look at the summary for the age variable
summary(GSS_2010$age) 

# Create new variable "new.age" with two levels "young" (if <= 45 yrs old) and old (if > 45 yrs old)
  
  # Method 1: ifelse function
GSS_2010$new.age <- ifelse(GSS_2010$age <= 45, # a logical conditional to test (here it's whether age is at most 45)
                           "young", # value to assign if condition is TRUE
                           "old") # value to assign if condition is FALSE
table(GSS_2010$new.age)
str(GSS_2010$new.age) # this tells us that new.age is an object of type "character" and it prints the first few values 

  # Method 2: recode function from memisc package
# install.packages("memisc")
library(memisc)
GSS_2010$new.age <- recode(GSS_2010$age, # the object we want to recode
                           "old" <- range(46, max), # assign value "old" if age is >= 46
                           "young" <- range(min, 45)) # assign value "young" if age is <= 45
str(GSS_2010$new.age) # we see that using recode() gives us a variable of type "factor" with two levels "old" and "young"
table(GSS_2010$new.age) # should be the same as output from Method 1
table(as.numeric(GSS_2010$new.age)) # this shows us that, by default, recode() sets the numeric values for "young" and "old" to be 1 and 2

# Create variable new.age but with numeric levels 1 (if <= 45 yrs old) and 0 (if > 45 yrs old)
GSS_2010$new.age <- ifelse(GSS_2010$age <= 45, 1, 0) # same as using ifelse() above but here we assign 1 and 0 instead of "young" and "old"
table(GSS_2010$new.age)


# Create variable new.sex, which changes the values of the sex variable from 1 and 2 to 0 and 1
table(GSS_2010$sex)

  # Method 1: using mapvalues function from plyr package
GSS_2010$new.sex <- mapvalues(GSS_2010$sex, # object to modify
                              from = c(1,2), # vector of the values we want to replace
                              to = c(0,1)) # vector of the new values we want to use
table(GSS_2010$new.sex)

  # Method 2: subtraction
GSS_2010$new.sex <- GSS_2010$sex - 1 # since we want to go values 1 and 2 to values 0 and 1 we can just subtract 1 from the value of every observation 
table(GSS_2010$new.sex)



# Factor/categorical variables
GSS_2010$new.sex2 <- factor(GSS_2010$sex, 
                            labels = c("Male", "Female"))
table(GSS_2010$new.sex2)
class(GSS_2010$new.sex2)
levels(GSS_2010$new.sex2) # check what the levels are
str(GSS_2010$new.sex2) # gives both the class and the levels




# Logical/Conditional expressions -----------------------------------------
# _________________________________________________________________________

# Various ways to compute the sum of the ages of everyone 45 and under
sum(subset(GSS_2010, 
           subset = age <= 45, # use obs for which age <= 45
           select = age)) # use only the age variable

sum(GSS_2010$age[GSS_2010$age <= 45], na.rm = T) # [GSS_2010$age <= 45] tells R to only use obs for which value of age is <=45
with(GSS_2010, sum(age[age <= 45], na.rm = T)) # same but with(GSS_2010, ...) allows us to avoid putting GSS_2010$ in front of every variable name
with(GSS_2010, sum(age[new.age == 1], na.rm = T)) # same but using our new.age variable from above (which is coded 1 if 45 or under)


# Various ways to count the number of people 45 and under
sum(GSS_2010$age <= 45, na.rm = TRUE) # GSS_2010$age <= 45 gives TRUE or FALSE for each element and these are treated as 1s and 0s by the sum function
sum(GSS_2010$new.age == 1, na.rm = TRUE) # same but using our new.age variable
length(which(GSS_2010$age <= 45)) # by getting the number of elements in the vector of indices for rows where age <= 45 
length(which(GSS_2010$new.age==1)) # same but using our new.age variable

# Compute the average age of people with exactly 2 siblings
mean(GSS_2010$age[GSS_2010$sibs == 2], na.rm = T)
with(GSS_2010, mean(age[sibs == 2], na.rm = T))

# Compute the average age of people with exactly 2 or 6 siblings
mean(GSS_2010$age[GSS_2010$sibs == 2 | GSS_2010$sibs == 6], na.rm = T) # the vertical bar "|" means "or"
with(GSS_2010, mean(age[sibs == 2 | sibs == 6], na.rm = T)) 
with(GSS_2010, mean(age[sibs %in% c(2,6)], na.rm = T))  

# Compute the average age of males with exactly 2 siblings 
mean(GSS_2010$age[GSS_2010$sibs == 2 & GSS_2010$sex == 1], na.rm = T) # the ampersand symbol "&" is used for "and"
with(GSS_2010, mean(age[sibs == 2 & sex == 1], na.rm = T)) 

# Count the number males with exactly 2 siblings 
with(GSS_2010, sum(sibs == 2 & sex == 1, na.rm = T))
