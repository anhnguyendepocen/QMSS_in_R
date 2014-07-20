#===============================#
#====== FIRST DIFFERENCES ======#
#===============================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014


# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load packages
library(QMSS)
library(plyr)
library(psych)
library(VGAM)

# load GSS panel data 
load("GSS_panel.RData")




# "Naive" models with panel data ------------------------------------------
# _________________________________________________________________________

# sort data by idnum and panelwave (using "arrange" function from plyr package)
pd <- arrange(pd,idnum,panelwave) # equivalent to pd <- pd[with(pd, order(idnum, panelwave)),] 

# make recoded version of "divlaw" variable called "divorce.easier"
pd$divorce.easier <- mapvalues(pd$divlaw,
                                from = 1:3,
                                to = c(3,1,2))

# quick (crude) check that recode worked (look at first 20 rows)
with(pd, table(divlaw, divorce.easier))

# get counts and percentages for levels of divorce.easier using custom tab function
Tab(pd$divorce.easier)

# make indicator variable for "divorced"
pd$divorced <- as.numeric(pd$marital == 3 | pd$marital == 4)
Tab(pd$divorced)

### Simple linear regression (OLS) ###
lm.divorce <- lm(divorce.easier ~ divorced, data = pd)
summary(lm.divorce)

# Add some control variables
divorce2.formula <- (divorce.easier ~ divorced + I(log(realinc)) + educ + 
                    as.factor(race) + as.factor(sex) + age)
lm.divorce2 <- lm(divorce2.formula, data = pd)
summary(lm.divorce2)

### Ordinal logit model ###
# using polr() from MASS package (can also use vglm() from VGAM package)
ologit.divorce <- polr(as.factor(divorce.easier) ~ divorced, data = pd)
summary(ologit.divorce)

### OLS with clustered standard errors ###

# first recompute OLS model lm.divorce using plm package (using model="pooling" option)

# plm is slow with big datasets so take a subset with variables we'll need 
vars <- c("idnum","panelwave","divorce.easier","divorced","realinc","educ","race","sex","age")
pd.sub <- pd[, vars] # same as pd.sub <- subset(pd, select = vars)

lm.divorce3 <- plm(divorce.easier ~ divorced, data = pd.sub, index = c("idnum", "panelwave"),
                   model = "pooling") # model = "pooling" will give us the same results as using lm()
summary(lm.divorce3) # should be same as lm.divorce

# calculate degrees of freedom adjustment
row.ids <- as.numeric(rownames(model.frame(lm.divorce3)))
  # 1. get number of clusters (omitting individuals with missingness on "divorce.easier" and/or "divorced")
n <- length(unique(pd.sub$idnum[row.ids]))
n <- with(pd.sub, length(unique(idnum[row.ids])))
  # 2. get number of observations (again omitting the same individuals with missingness)
N <- length(row.ids)
  # 3. compute degrees of freedom
df <- (n/(n - 1)) * (N - 1)/lm.divorce3$df.residual
# retest coefficients using coeftest() from lmtest package
coeftest(lm.divorce3, vcov = df*vcovHC(lm.divorce3, type = "HC0", cluster = "group"))

# For future use I've combined these steps in a function clusterSE
?clusterSE
clusterSE(fit = lm.divorce3, cluster.var = "idnum", data = pd.sub)




# First Differences -------------------------------------------------------
# _________________________________________________________________________


### First differences model ###
plm.divorce <- plm(divorce.easier ~ divorced + panelwave, # model formula
                   index = c("idnum", "panelwave"), # id & time variables
                   model = "fd", # "fd" for first differences
                   data = pd.sub)
summary(plm.divorce)


# Compare adjusted R^2 from first differences and naive OLS models
#   note: for models fit with plm the adjusted R^2 can be extracted using summary(model)$r.squared[2]
#   whereas for models fit with lm we can use summary(model)$adj.r.squared
Adj.R2 <- c(summary(plm.divorce)$r.squared[2],
            summary(lm.divorce)$adj.r.squared) 
names(Adj.R2) <- c("FD", "OLS")
round(Adj.R2, 4)
            

### Add control variables ###

# if we have a lot of variables to include in a model then it can be nice to keep things a bit cleaner visually like this:
y.var <- 'divorce.easier' # the y variable
x.vars <- c('divorced','I(log(realinc))','educ','as.factor(race)','as.factor(sex)','age','panelwave') # the x variables
paste(y.var, '~', paste(x.vars, collapse=' + ' )) # paste everything together into formula (collapse=' + ' inserts a + between the variables in x.vars)

# we can use as.formula() to make this text of the formula a formula object to use in the plm function
plm.formula <- as.formula( paste(y.var, '~', paste(x.vars, collapse=' + ' )))

# then to run our model we can just write:
plm.divorce2 <- plm(plm.formula, index=c("idnum", "panelwave"), model="fd", data=pd.sub)
summary(plm.divorce2)


# create first-differenced variable "d.sex" and add it to our subset of the data
pd.sub2 <- ddply(pd.sub, # the dataset
                "idnum", # split the data by "idnum" variable 
                mutate, # funtion to apply to each "idnum" subset (mutate tells ddply that the next argument will be a new variable created as function of an existing variable)
                d.sex = firstD(sex)) #create new variable "d.sex" using custom function firstD in QMSS package


# create first-differenced variable "d.age" 
pd.sub <- ddply(pd.sub, "idnum", mutate, d.age = firstD(age))
summary(pd.sub$d.age)

# create indicators for race==2, race==3
pd.sub$race2 <- pd.sub$race==2
pd.sub$race3 <- pd.sub$race==3

with(pd.sub, table(race3, race, useNA = "ifany"))

# create differenced versions of the race, divorce.easier, divorced, and educ variables
pd.sub <- ddply(pd.sub, "idnum", mutate, 
            d.race2 = firstD(race2), 
            d.race3 = firstD(race3),
            d.divorce.easier = firstD(divorce.easier),
            d.divorced = firstD(divorced),
            d.educ = firstD(educ))

### Re-run the regression requiring constant variable to not change across waves ###
# logical expression to use when subsetting the data
constants <- with(pd.sub, d.age==2 & d.sex==0 & d.race2==0 & d.race3==0)
# run the model on the subset of data where the "constant" vars don't change across waves
lm(d.divorce.easier ~ d.divorced + I(log(realinc)) + d.educ + d.race2 + 
     d.race3 + d.sex + d.age, data = pd.sub, subset = constants)


# Differences in Xs often have much less variance than the distribution of original Xs
describe(pd.sub[,c("divorce.easier", "d.divorce.easier", "divorced", "d.divorced")])

### Re-run regression, only including the "changers" ###
plm.divorce3 <- plm(d.divorce.easier ~ d.divorced + panelwave, 
                    model = "pooling", data = subset(pd.sub, d.divorced!=0))
# retest with clustered se
clusterSE(fit = plm.divorce3, cluster.var = "idnum", data = subset(pd.sub, d.divorced!=0))

### Ordinal first differences ###
vglm.divorce <- vglm(as.ordered(d.divorce.easier) ~ d.divorced + panelwave, 
                     data = pd.sub, family = propodds)
summary(vglm.divorce)

# Use our custom prop.odds.test function
vglm.divorce2 <- vglm(formula(vglm.divorce),pd.sub,family=cumulative(reverse=T))
propOddsTest(vglm.divorce, vglm.divorce2)

# ￼Symmetry of differenced effects
  # using custom "stata.tab" function
stata.tab(d.divorce.easier, d.divorced, 
          data = subset(pd.sub, d.divorced!=0), 
          NAs = F, freq = T, col = T)



# Means of d.divorce.easier by level of d.divorced
  # like Stata's "tabstat ddivorceeasier, by(ddivorced)"
ddply(pd.sub,"d.divorced", summarize,
      Mean = mean(d.divorce.easier,na.rm=T))
  # or alternatively like this:
with(pd.sub, by(d.divorce.easier, d.divorced, mean, na.rm=T))

### Re-run model on sub-groups ###
# lower-educated people
plm.loweduc <- plm(d.divorce.easier ~ d.divorced, 
                     model = "pooling", 
                     data = subset(pd.sub, pd$degree<3))
# retest with clustered se
clusterSE(plm.loweduc, "idnum", subset(pd.sub, pd$degree<3))

# higher-educated people
plm.higheduc <- plm(d.divorce.easier ~ d.divorced, 
                    model = "pooling", 
                    data = subset(pd.sub, pd$degree>=3))
# retest with clustered se
clusterSE(plm.higheduc, "idnum", subset(pd.sub, pd$degree>=3))


