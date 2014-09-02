#==================================================#
#====== WRITING A SIMPLE TABULATION FUNCTION ======#
#==================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# load GSS data
load("GSS.RData")



# First: getting familiar with table, prop.table, and cumsum functions ----
# _________________________________________________________________________

# For a single variable X, table() returns a table of the counts at each level of X
  # (table is also used for cross-tabulations of multiple variables, but for now we'll just worry about 1 variable)
X <- c("yes","yes","no")
table(X)
X <- c(10, 10, 77, 99, 99, 99)
table(X)
X <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
table(X)

# The useNA argument controls whether or not NA values are included. The default is useNA = "no" (i.e. NAs will not be included) 
  # useNA = "ifany" will include NA's only if there are any
X <- c("yes","yes","no", NA, NA)
table(X)
table(X, useNA = "ifany") 

  # useNA = "always" will always includes NA's 
X <- c("yes","yes","no")
table(X)
table(X, useNA = "ifany")
table(X, useNA = "always") 


# When applied to a table, the prop.table function returns proportions instead of counts
prop.table(table(X))
prop.table(table(X, useNA = "always"))

# The cumsum function returns a vector of cumulative sums (i.e. the "running total")
X <- c(1, 2, 3)
cumsum(X) 
X <- 1:10
cumsum(X)




 

# Writing simple functions ------------------------------------------------
# _________________________________________________________________________

# Before writing a function combining the table, prop.table and cumsum functions, 
# we can write a few very very simple functions to learn about the syntax involved.
# The first function Mult will multiply two numbers and the second function Sub
# will subtract two numbers. We'd never write these functions when doing real work since 
# we can just use * and - to multiply and subract. These are just for practice. 

# Multiplication Function
Mult <- function(x, y) { # Mult is the function name, x and y are the function's arguments 
  x*y # multiply x and y together
}

# After running the lines of code defining Mult, we can then use the function to multiple two numbers
Mult(x = 2, y = 3) 
Mult(2,3) # we don't have to name the arguments 

# Subtraction Function
Sub <- function(a, b) { # it doesn't matter if we use a and b or x and y for the arguments as long as we use the same notation inside the function
  a - b
}

Sub(a = 2, b = 3) # subtracts 3 from 2
Sub(2, 3) # same 
Sub(b = 3, a = 2) # the same because even though we reverse the order we gave names to the arguments
Sub(3, 2) # NOT the same. Because they're unnamed, R will assume that a = 3 and b = 2

# We can also use functions we've written inside other functions. For example, we can write a new
# function that calls the functions Mult and Sub
MultSub <- function(Aching, Tornado) { # we can use arbitrary words for the arguments if we want (although it's usually best to use meaningful names)
  m <- Mult(Aching, Tornado)
  s <- Sub(Aching, Tornado)
  c(m, s) # combine into a vector
}

# We can add names to the vector to tell the user more about what the output means
MultSub2 <- function(Aching, Tornado) { 
  m <- Mult(Aching, Tornado)
  s <- Sub(Aching, Tornado)
  c(mult = m, sub = s) # add names
}

# Now compare the output of MultSub and MultSub2
MultSub(10, 20)
MultSub2(10, 20)


# We can also set default values for the arguments of a function. R will use the default values unless
# the user specifies different ones. 
MultSub3 <- function(Aching = 5, Tornado = 4) { # set defaults to 5 and 4
  m <- Mult(Aching, Tornado)
  s <- Sub(Aching, Tornado)
  c(mult = m, sub = s) 
}

# MultSub3 will now give exactly the same output as MultSub2 if the user gives values for both arguments
# but if the user omits one or both the arguments MultSub3 will use the default values
MultSub3(10, 20) # same as MultSub2
MultSub3(10) # R wil use the default value of 4 for the omitted argument
MultSub3(, 10) # R wil use the default value of 5 for the omitted argument
MultSub3() # R will use the default values for both arguments



# Writing a simple tabulation function ------------------------------------
# _________________________________________________________________________

# Using table, prop.table, and cumsum we can write a very simple function to return
# a table of counts, percentages, and the cumulative percentages 

# we'll call the function "Tab" and give it 3 arguments:
  # var: the name of the variable we want to use
  # digits: an integer specifying the number of decimal places to use (for rounding the percentages)
    # we'll set digits to be 2 by default but the user can change it to a different integer
  # useNA: should NA values be included?
    # we'll set this to "no" by default but can be changed to "ifany" or "always"

Tab <- function(var, digits = 2, useNA = c("no", "ifany", "always")) {
  # first get the counts (with or without NAs depending on useNA argument) 
  Count <- table(var, useNA = useNA) 
  # then get percents by multiplying proportions by 100
  Pct <- 100*prop.table(Count) 
  # now the cumulative percentage 
  Cum.Pct <- cumsum(Pct)
  # finally, use cbind() to combine Count, Pct, and Cum.Pct by columns
  # and round() to round to the specified number of digits
  cbind(Count, Pct = round(Pct, digits), Cum.Pct = round(Cum.Pct, digits)) 
}

# Test our function
X <- c("yes","yes", "yes", "no", "maybe", "maybe", "maybe", NA, NA, NA, NA)
Tab(X) # by default it will use digits = 2 and useNA = "no"
Tab(X, digits = 3, useNA = "ifany") # now round to 3 decimal places and include the NAs

# Test our function on the GSS data
Tab(GSS$sex)
Tab(GSS$marital, digits = 4, useNA = "ifany")

# For future use, the Tab function is included in the QMSS package
library(QMSS)
?Tab