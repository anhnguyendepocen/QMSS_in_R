#=================================#
#====== MULTIPLE REGRESSION ======#
#=================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load some packages
library(QMSS)
library(plyr)

# Load the GSS cumulative dataset & 2010 subset
load("GSS.RData")
load("GSS_2010.RData")



# Does owning a house increase your vocabulary? ---------------------------
# _________________________________________________________________________

# Take a subset of the cumulative GSS data including only the dwelown, wordsum, 
# degree variables. The cumulative file is huge so taking a subset can help
# speed up computations
sub <- GSS[, c("dwelown", "wordsum", "degree")] 
Tab(sub$dwelown)

# Create indicators for the different levels of dwelown
sub$home1 <- sub$dwelown == 1
sub$home2 <- sub$dwelown == 2
sub$home3 <- sub$dwelown == 3

# If there were many more than 3 levels it could be more efficient to use a loop
# along with the assign function.
?assign
sub <- within(sub, for(i in 1:3) {
  assign(paste0("home",i), dwelown == i) 
  })

# Simple Linear Regression Model
summary(lm(wordsum ~ home1, data = sub, !is.na(degree)))

# Multiple Regression Model
summary(lm(wordsum ~ home1 + degree, data = sub))

# Mean of wordsum by levels of home1
  # using ddply() from plyr package
ddply(.data = na.omit(sub), # na.omit(sub) omits anyone with NAs on any of the vars in sub
      "home1", summarise, Mean.wordsum  = mean(wordsum))
  # or using base R function tapply
with(na.omit(sub), tapply(wordsum, home1, mean))

# Mean of wordsum by levels of degree and home1 
ddply(.data = na.omit(sub), c("degree", "home1"), summarise, Mean.wordsum = mean(wordsum))
  # Or with base R functions: 
with(na.omit(sub), tapply(wordsum, list(home1, degree), mean))
with(na.omit(sub), by(wordsum, list(home1, degree), mean))


# Bar graph of differences in avg wordsum score btw home-owners and
# non-home-owners by degree level

  # first get overall difference in avg wordsum scores btw home-owners and
  # non-home-owners
total.diff <- with(na.omit(sub), mean(wordsum[home1==T]) - mean(wordsum[home1==F]))
total.diff

  # now by degree level
wordsum.tab <- ddply(na.omit(sub), "degree", summarise,
                     diff = mean(wordsum[home1==T]) - mean(wordsum[home1==F]))
wordsum.tab

  # add the overall difference as last row of the table using rbind()
wordsum.tab <- rbind(wordsum.tab, c(degree = 5, diff = total.diff))
wordsum.tab

  # Now make the bar graph 
barplot(as.numeric(wordsum.tab[, "diff"]), # heights of the bars
        names.arg = c(0:4, "overall"), # labels for the bars
        col = rainbow(n=6, v=.75)) # color bars with six color rainbow (see ?rainbow())

  # or using ggplot2 package
# install.packages("ggplot2") 
library(ggplot2)
g <- ggplot(wordsum.tab, 
            aes(x = degree, y = diff, fill = factor(degree, labels = c(0:4, "overall")))) 
g <- g + geom_bar(stat="identity") # "identity" means don't apply a statistical transformation, i.e. just give us the value of "diff" 
(g <- g + guides(fill = guide_legend(title = "DEGREE")))



# Educational attainment & father’s religious attendance ------------------
# _________________________________________________________________________

# Make a new subset of the cumulative GSS 
sub <- GSS[, c("educ", "paattend", "paeduc")]
summary(sub)

# Gets counts and percentages for the levels of paattend
with(sub, Tab(paattend))
with(sub, Tab(paattend, useNA = "ifany")) 


# Simple Linear Regression
summary(lm(educ ~ paattend, data = sub, subset = !is.na(paeduc)))

# If we didn't put subset = !is.na(paeduc)
summary(lm(educ ~ paattend, data = sub))

# Multiple regression
summary(lm(educ ~ paattend + paeduc, data = sub))

# Relationship between paattend and paeduc
summary(lm(paattend ~ paeduc, data = sub))

# Quick note about R: if we had assigned the original simple linear regression
# model to an object
lm.educ <- lm(educ ~ paattend, data = sub, subset = !is.na(paeduc))
# then we when we expand to the multiple regression model we can use the 
# update() function (see ?update) to modify lm.educ by adding paeduc to the 
# independent variables
lm.educ2 <- update(lm.educ, ~ . + paeduc) # this adds "+ paeduc" to the right side of the original model formula
# Or, instead of creating a new object lm.educ2 we could just replace our
# original model if we wanted
lm.educ <- update(lm.educ, ~ . + paeduc)



# Does marriage make you smarter? -----------------------------------------
# _________________________________________________________________________

sub <- GSS[, c("marital", "wordsum", "degree")]
Tab(sub$marital, useNA = "ifany") 

# Make indicator variable for marital=1 (married)
sub$married <- sub$marital == 1
Tab(sub$married, useNA = "ifany")

# Simple linear regression
summary(lm(wordsum ~ married, data = sub, subset = !is.na(degree)))

# Multiple regression
summary(lm(wordsum ~ married + degree, data = sub))

# With education and spouse's education  
sub <- GSS[, c("wordsum", "educ", "speduc")]
summary(lm(wordsum ~ educ + speduc, data = sub))



# Occupational prestige & father's occupational prestige ------------------
# _________________________________________________________________________

sub <- GSS[, c("educ", "prestg80", "papres80")]

# Simple linear regression
summary(lm(prestg80 ~ papres80, data = sub, subset = !is.na(educ)))

# Multiple regression 
summary(lm(prestg80 ~ papres80 + educ, data = sub))



# Family size & standardized coefficients ---------------------------------
# _________________________________________________________________________

#Simple linear regression
lm.family <- lm(childs ~ sibs, data = GSS_2010, !is.na(age))
summary(lm.family)

# Multiple regression
lm.family2 <- update(lm.family, ~ . + age)
summary(lm.family2)

# Write a function to compute standardized regression coefficients (for practice with 
# custom functions)
  # STEP 1. Name the function and its argument(s) (we'll use stdCoef as the function name,
    # and "fit" as the name of the argument since we'll apply our function to a fitted model)
  # STEP 2. Get the standard deviations of all of the variables in the model by applying
    # the sd() function to each column of fit$model (fit$model gives us a dataframe only 
    # containing the vars and obs used when running the model)
  # STEP 3. Extract the coefficients from the model except for the intercept
  # STEP 4. Compute the standardized coefficients as std.coeff = coef * sx/sy
  # STEP 5. (Optional) Have R print an informative message about the function's output

# Step 1. 
stdCoef <- function(fit){
# Step 2. 
  sd <- apply(X = fit$model, MARGIN = 2, FUN = sd) # MARGIN = 2 indicates columns (for rows we would use 1)
# Step 3.
  coefficients <- fit$coef[-1] # the [-1] tells R not to include the first coefficient (which is the intercept)
# Step 4. 
  std.coefs <-coefficients*(sd[-1]/sd[1]) # [-1] excludes the first element in sd (which is the y-var so it just gives us the sd's of the x-vars) 
                                          # [1] includes only the first element in sd (so just the y-var)
# Step 5. 
  cat("Standardized Coefficients for ", deparse(substitute(fit)), "\n") # deparse(substitute(fit)) takes the argument of our function "fit" and converts it to a character string
                                                                        # "\n" inserts a linebreak
  return(std.coefs) # this tells R that the output should be std.coefs
  }

# Use our new stdCoef() function to get the standardized coefficients for our
# multiple regression model
stdCoef(lm.family2)



# Creat dummy variable for male
Tab(GSS_2010$sex)
GSS_2010$male <- GSS_2010$sex == 1

# Multiple regression including the dummy for male
lm.family3 <- update(lm.family2, ~ . + male)
stdCoef(lm.family3) # get standardized coefficients




# On dummy variables ------------------------------------------------------
# _________________________________________________________________________

Tab(GSS_2010$region)
class(GSS_2010$region) # right now "region" is of class "integer"

# Simple linear regression
summary(lm(educ ~ region, data = GSS_2010)) # treating region as a numeric variable

# Simple linear regression with dummies for region
  # instead of creating a separate dummy variable for each level of region we
  # can use factor()
summary(lm(educ ~ factor(region), data = GSS_2010)) 

# We could also make a new variable that is of class "factor"
GSS_2010$f.region <- factor(GSS_2010$region)
class(GSS_2010$f.region) 
summary(lm(educ ~ f.region, data = GSS_2010)) # now we don't have to use as.factor() 

# We can label the levels of f.region 
levels(GSS_2010$f.region) # display the levels (i.e. the values it takes on)
region.labels <- c("NewEngland", "MiddleAtlantic", "E.Nor.Central", 
                   "W.Nor.Central", "SouthAtlantic", "E.Sou.Central",
                   "W.Sou.Central", "Mountain", "Pacific")
levels(GSS_2010$f.region) <- region.labels
levels(GSS_2010$f.region)
summary(lm(educ ~ f.region, data = GSS_2010)) # these are now printed when we run the model

# Changing the reference/base category with relevel()
summary(lm(educ ~ f.region, data = GSS_2010)) # by default level 1 (New England) is omitted
# If we wanted to change the reference category to 9 (Pacific) we could:
  # Option 1: do it inside the model without changing the underlying dataset
summary(lm(educ ~ relevel(f.region, ref=9), data = GSS_2010)) # omit level 9 (pacific) instead
summary(lm(educ ~ relevel(f.region, ref="Pacific"), data = GSS_2010)) # equivalent

  # Option 2: change the default reference level for f.region inside the data
  # set and then run the model
GSS_2010$f.region <- relevel(GSS_2010$f.region, ref = 9) # or ref = "Pacific"
summary(lm(educ ~ f.region, data = GSS_2010)) # now we don't need to relevel inside the model




# Inside multiple regression ----------------------------------------------
# _________________________________________________________________________

# install.packages("scatterplot3d")
library(scatterplot3d)

# define a function to take a random sample of GSS_2010 without missingness on
# educ and prestg80 and with educ at least 10 (for demonstration purposes)
sample_dat <- function(rng.seed = sample(.Machine$integer.max, 1)){
  # sample(.Machine$integer.max, 1) will pick a random integer between 1 and the
  # largest integer that can be represented on the machine
  
  # set the seed for the pseudo random number generator. by default this will be
  # sample(.Machine$integer.max, 1), but for reproducibility purposes we can
  # specify rng.seed to be whatever we want
  set.seed(rng.seed)
  
  # For demonstration purposes only, take subset of GSS_2010 without missingness
  # on educ and prestg80 and with educ at least 10
  sub <- subset(GSS_2010, educ>=10 & !is.na(educ) & !is.na(prestg80), 
                select = c(prestg80, educ, sex))
  # Take a random sample of 10 individuals (for demonstration purposes)
  samp <- sub[sample(nrow(sub),10), ]
  
  # Make indicator for male
  samp$male <- samp$sex == 1

  # We don't need to do this, but for future reference if we wanted to keep the
  # male variable but drop the original sex variable we can use NULL like
  samp$sex <- NULL
  
  return(samp)
}  

# define a function to create a 3D scatterplot and add a fitted plane using the
# output from the sample_dat function
scatter3d_my_sample <- function(samp){
  # 3d scatter plots 
  scatter_3D <- with(samp, scatterplot3d(educ,male,prestg80, pch=16, highlight.3d=TRUE,
                                         type="h", main="3D Scatterplot"))
  # add a fitted plane
  plane <- lm(prestg80 ~ educ + male, data = samp) 
  scatter_3D$plane3d(plane, lty.box = "solid")
}

# we can now run the scatter3d_my_sample function with the sample_dat function
# as the argument to get a 3D scatter plot and fitted plane. try running it
# several times to see the plots corresponding to different random samples

scatter3d_my_sample(sample_dat()) 
scatter3d_my_sample(sample_dat())
scatter3d_my_sample(sample_dat())
scatter3d_my_sample(sample_dat())


# now store a sample to use for the rest of the demonstration
samp <- sample_dat(rng.seed = 123456) # setting the seed for reproducibility
scatter3d_my_sample(samp)


# Correlation matrix
corrs <- cor(samp) # if we hadn't dropped the sex variable we could use cor(samp[,c("educ","male","prestg80")]) to include male but not sex
corrs

# Means and SDs
means <- sapply(samp, mean) # or colMeans(samp)
means
SDs <- sapply(samp, sd)
SDs

# Partial slopes for educ and male
  # use y, x1, and x2 as shorthand 
y <- "prestg80"; x1 <- "educ"; x2 <- "male"
b <- c(educ = NA, male = NA)
b[1] <- SDs[y]/SDs[x1]*(corrs[y,x1] - corrs[y,x2]*corrs[x1,x2])/(1 - corrs[x1,x2]^2)
b[2] <- SDs[y]/SDs[x2]*(corrs[y,x2] - corrs[y,x1]*corrs[x1,x2])/(1 - corrs[x1,x2]^2)
round(b, 3)

# Check that we got the same values as if we had used lm()
lm(prestg80 ~ educ + male, data = samp) 

# Purging education of male
  # Get residuals from regression of educ on male
samp$e1 <- lm(educ ~ male, data = samp)$residuals
print(samp[,c("educ","male","e1")], row.names = F)

  # Get correlations between residuals and the two independent variables
with(samp, cor(e1, cbind(educ,male)))

  # Estimate the effect of education (purged of maleness) on prestige
summary(lm(prestg80 ~ e1, data = samp)) # coef on e1 should be same as coef on educ in the multiple regression model

# Purging male of education
  # Get residuals from regression of educ on male
samp$e2 <- lm(male ~ educ, data = samp)$residuals
print(samp[, c("educ", "male", "e2")], row.names = F)
 
  # Get correlations between residuals and the two independent variables
with(samp, cor(e2, cbind(educ,male)))

  # Estimate the effect after "purging"
summary(lm(prestg80 ~ e2, data = samp)) # coef on e2 should be same as coef on male in the multiple regression model


# R^2 in multiple regression
  # compute R^2 
R2 <- (SDs[x1]/SDs[y] * b[x1] * corrs[y,x1] 
       + SDs[x2]/SDs[y] * b[x2] * corrs[y,x2] )
names(R2) <- "R2"
R2
  # Check that we got the same R^2 as if we had extraced it from the model
summary(lm(prestg80 ~ male + educ, data = samp))$r.squared




# Do trust levels vary by race? -------------------------------------------
# _________________________________________________________________________

# New subset of cumulative GSS dataset
vars <- c("trust", "race", "realinc", "educ", "region")
sub <- GSS[, vars]

# Recodes
sub$new.trust <- mapvalues(sub$trust, from = c(1,2,3), to = c(3,1,2))
sub$black <- sub$race == 2

# Simple bivariate model
lm.trust <- lm(new.trust ~ black, data = sub, 
               subset = !is.na(educ) & !is.na(realinc) & !is.na(region))
summary(lm.trust)

# Global F-test
  # We can see the F-statistic from the output of summary(lm.trust) above or we
  # can extract it
summary(lm.trust)$f

# Add in more predictors
  # Below, I(log(realinc)) means use log(realinc) as the variable w/o having to 
  # create a new variable, i.e. no need to do lnrealinc <- log(realinc)).
lm.trust2 <- update(lm.trust, ~ . + educ + I(log(realinc)) + as.factor(region))
summary(lm.trust2)

# Partial F-test (restricted model vs unrestricted model)
anova(lm.trust, lm.trust2) 

# Adjusted R^2 from the unrestricted model
summary(lm.trust2)$adj.r.sq




# Collinearity, heteroskedasticity, normality of errors, outliers ---------
# _________________________________________________________________________

### Collinearity ###
summary(lm(tvhours ~ age + age, data = GSS_2010)) # the 2nd age variable is just dropped


### Heteroskedasticity ###
lm.tv <- lm(tvhours ~ degree, data = GSS_2010)
summary(lm.tv)

# Breusch-Pagan test for heteroskedasticity using bptest() from lmtest package
bptest(lm.tv) 

# Boxplots of residuals by degree level
e <- lm.tv$residuals # extract residuals 
deg <- lm.tv$model$degree # extract the obs of degree used in fitting the model

  # using traditional R boxplot() function 
boxplot(e ~ factor(deg))
  # if we don't have the levels of degree named, we can pass an optional "names"
  # argument to boxplot()
names <- c("<HS","HS","JrCol","BA",">BA")
boxplot(e ~ factor(deg), names = names)

  # or can do a boxplot using ggplot 
ggplot(NULL, aes(x = factor(deg, labels = names), y = e)) + geom_boxplot() + xlab("DEGREE")


# Robust standard errors
# install.packages("rms")
library(rms)
# we use the robcov() and ols() functions in the rms package to get Huber-White
# estimator of covariance matrix the values reported in the S.E. column are now
# robust standard errors
robcov(ols(tvhours ~ degree, data = GSS_2010, x = T, y = T)) # need args x=T and y=T (so robcov() has access to expanded design matrix)



### Normality of errors ###

# Histogram of the residuals

  # using truehist() from MASS package
truehist(e, xlab = "Residuals", col = "skyblue", border = "white", ylim = c(0, 0.5)) 
curve(dnorm, add=T, col = "maroon", lty = 2) # add the standard normal curve to the histogram plot (dnorm is R's normal density function. without specifying additional arguments the default is standard normal)

  # using R's traditional hist() function 
hist(e, prob = T) # note: can also modify the color, border, labels, etc  



# Quantile-Quantile plot (residuals against normal distribution)
qqnorm(e)

# Shapiro-Wilk test of normality
shapiro.test(e)



### Outliers & influential observations ###

# Residuals vs. leverage plot
  # just using plot(lm.tv) gives a bunch of plots. the "which" argument allows us
  # to choose a particular plot without plotting all of them
plot(lm.tv, which = 5) 

# Cooks distance
cooks.distance(lm.tv)
head(sort(cooks.distance(lm.tv), decreasing = T), 5) # just the highest 5 values




# Robust regression -------------------------------------------------------
# _________________________________________________________________________

# Use rlm() from MASS package 
summary(rlm(tvhours ~ degree, data = GSS_2010))




# Quantile regression -----------------------------------------------------
# _________________________________________________________________________
# install.packages("quantreg")
library(quantreg)
summary(rq(tvhours ~ degree, data = GSS_2010)) 

