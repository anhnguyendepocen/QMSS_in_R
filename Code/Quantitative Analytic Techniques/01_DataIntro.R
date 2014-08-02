#========================#
#====== DATA INTRO ======#
#========================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014




# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# To see the current working directory use getwd()
getwd() 

# To set a different working directory use setwd()
setwd("INSERT PATH TO DIRECTORY") # replace text inside quotes with path to the desired directory

# Load GSS cumulative dataset 
load("GSS.RData") # this assumes GSS.RData is in the working directory. 

# If GSS.RData data is in a subdirectory of the working directory then you can use the relative path
# to that subdirectory. If the file is not in the working working directory then the full path is needed.

# The cumulative dataset has data for many years. One way to extract a subset for a particular year
# is to use the subset function
GSS_2010 <- subset(GSS, year == 2010) # only data for 2010

# We can save the GSS_2010 subset to the working directory so we can use it without also loading 
# the larger GSS dataset
save(GSS_2010, file = "GSS_2010.RData")

# In the future we can load the GSS cumulative dataset or 2010 subset using the load() function
load("GSS_2010.RData")



# Inspect and describe the data -------------------------------------------
# _________________________________________________________________________

# Get some summary statistics for the variable called "health"
summary(GSS_2010$health) # GSS_2010 is the data frame; health is the column/variable

summary(GSS_2010[,'health']) # this gives the same output as "summary(GSS_2010$health)"
# Between the brackets we can put a row name/number before the comma and column name/number
# after the comma. Here we want to summarize the "health" variable for all rows, so we leave
# the row index blank

# To get a summary of two variables "health" and "sibs" we can use the c() funcion, which
# combines its arguments into a vector 
summary(GSS_2010[, c("health", "sibs")]) 

# Print the first 10 values of health variable
head(GSS_2010$health, n = 10) # the value of n determines how many values are printed

# Print the last 10 values of health variable
tail(GSS_2010$health, n = 10) 

# Count the number of rows in the dataset
nrow(GSS_2010) 

# Count the number of columns (variables) in the dataset
ncol(GSS_2010)

# Print the row numbers containing the missing values for the health variable
which(is.na(GSS_2010$health)) 

# For each observation of health print TRUE if the element is missing or FALSE if not
is.na(GSS_2010$health) 

# Count the number of missing values for the health variable
sum(is.na(GSS_2010$health)) # TRUE is counted as 1 and FALSE as 0 
length(which(is.na(GSS_2010$health))) # same as sum(is.na(GSS$health)) because this gives the length of the vector containing the only the row numbers of the missing observations

# Count the number of non-missing observations
sum(!is.na(GSS_2010$health)) # !is.na() is the logical negation of is.na()

# View the counts for the different values of the sibs variable
table(GSS_2010$sibs) 

# Compute the (arithmetic) mean of the sibs variable
mean(GSS_2010$sibs, na.rm = TRUE) # the argument na.rm = TRUE tells R to ignore any missing values
mean(GSS_2010$sibs) # output will be "NA" if there are any missing values or, if no missings, it's the same as mean(GSS_2010$sibs, na.rm = TRUE)

# Compute other summary statistics for sibs variable
var(GSS_2010$sibs, na.rm = T) # variance
sd(GSS_2010$sibs, na.rm = T) # standard deviation
min(GSS_2010$sibs, na.rm = T) # minimum value
max(GSS_2010$sibs, na.rm = T) # maximum value
range(GSS_2010$sibs, na.rm = T) # range (this gives both the min and max)
quantile(GSS_2010$sibs, na.rm = T) # quantiles (default is to give 0%,25%,50%,75%,100% quantiles)
quantile(GSS_2010$sibs, probs = c(1/3, 2/3), na.rm = T) # can add probs argument to get any quantiles you want

# You can pull out single statistics by indexing with brackets
quantile(GSS_2010$sibs, na.rm = T)[2] # just the 25% quantile

# Cross-tabulation of counts for happy and health variables
table(GSS_2010[, c("happy","health")])
with(GSS_2010, table(happy, health))
table(GSS_2010[, c("happy","health")], useNA = "ifany") # also tabulate any NA values

# Cross-tabulation of happy and health with proportions instead of counts
prop.table(table(GSS_2010[, c("happy","health")]))




# Install, load and use a package -----------------------------------------
# _________________________________________________________________________

# Install the "plyr" package (with tools for splitting, applying and combining data)
install.packages("plyr") # A great package for summarizing data and creating new variables that are functions of variables in the data

# Load the package.
library(plyr) # this needs to be done for each R session

# Use plyr's ddply() function to get mean number of siblings by region
ddply(GSS_2010, # the data frame
      "region", # the name of the variable we want to use to subset the data. can also use the syntax .(region) instead of "region"
      summarize, # the function to apply to each subset
      Mean.Sibs = mean(sibs, na.rm = T) # display the mean in a column called "mean(sibs)" 
      )

# Same but also including standard deviation and median 
ddply(GSS_2010, "region", summarize, 
      Mean = mean(sibs, na.rm = T),
      SD = sd(sibs, na.rm = T),
      Median = median(sibs, na.rm = T))

# Same but assign the results to a data frame called "Regional"
Regional <- ddply(GSS_2010, "region", summarize, 
                  Mean = mean(sibs, na.rm = T),
                  SD = sd(sibs, na.rm = T),
                  Median = median(sibs, na.rm = T))
Regional # view the results
