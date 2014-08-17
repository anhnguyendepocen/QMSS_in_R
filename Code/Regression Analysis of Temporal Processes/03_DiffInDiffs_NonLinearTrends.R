# To-do:
  # make ggplots commands less wordy



#===========================================================#
#====== DIFFERENCE-IN-DIFFERENCES, NON-LINEAR TRENDS  ======#
#===========================================================#

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



# Difference-in-differences -----------------------------------------------
# _________________________________________________________________________


### Does opposite-party president affect Republican happiness more than Democratic happiness? ###

vars <- c("happy", "partyid", "year", "age", "educ", "sex", 
          "realinc", "polviews", "race", "region")
sub <- GSS[, vars]
Tab(sub$happy)
sub$n.happy <- ReverseThis(sub$happy)
with(sub, table(happy, n.happy))

Tab(sub$partyid)
sub$repub <- mapvalues(sub$partyid,
                       from = 0:7,
                       to = c(rep(0,3), NA, rep(1,3), NA))
Tab(sub$repub)

lm.president <- lm(n.happy ~ as.factor(year)*repub, data = sub, year==2006 | year==2010)
summary(lm.president)


# with controls
lm.president2 <- update(lm.president, ~ . + age + educ + sex + realinc 
                        + polviews + as.factor(race) + as.factor(region))
summary(lm.president2)


# Plot trend of mean happiness score, Republicans vs. Democrats, over time
happy.by.year <- ddply(sub, c("year", "repub"), summarise, mean = mean(n.happy, na.rm = T))
happy.by.year <- na.omit(happy.by.year)
plot_colors <- scale_color_manual(values = c("blue", "red"), name = "repub") 

  # linear fit for years 2006 to 2010
g_happy_06to10 <- ggplot(subset(happy.by.year, year %in% 2006:2010), 
                         aes(x = year, y = mean, group = repub, color = factor(repub)))
g_happy_06to10 + plot_colors + stat_smooth(method = "lm", se = F) 

  # for all available years, actual trend with linear fit
g_happy_all <- ggplot(happy.by.year, aes(x = year, y = mean, 
                                         group = repub, color = factor(repub)))
g_happy_all + plot_colors + stat_smooth(method = "lm", se = F, lty = 2) + geom_line()



# Non-Linear Time Trends --------------------------------------------------
# _________________________________________________________________________

# Load our saved APC work
load("APC.RData")

# overall natcrime trend
g_year + ylim(2.45, 2.75) 

# add fitted line
(g_year <- g_year + stat_smooth(method = lm, se = FALSE, color = "maroon", lty = 2, lwd = 1.25))


 # We can test for the need for higher-order terms
lm.natcrime_test <- lm(n.natcrime ~ year, data = sub)
summary(lm.natcrime_test)
resettest(lm.natcrime_test, power = 2:4) # Ramsey RESET test

### Quadratics and cubics ###

# Add a quadradic term
lm.natcrime_quad <- update(lm.natcrime_test, ~ . + I(year^2))
summary(lm.natcrime_quad)
round(coef(lm.natcrime_quad), 3)

# Did adding a squared term solve our problem?
resettest(lm.natcrime_quad, power = 3:4)

# Add a cubic term
lm.natcrime_cub <- update(lm.natcrime_quad, ~ . + I(year^3))
summary(lm.natcrime_cub)
round(coef(lm.natcrime_cub), 3)

# What do the quadratic & cubic trends on nnatcrime look like?
g_quad_cubic <- ggplot(by.year, aes(x = year, y = NATCRIME)) + geom_point() 
  # quadratic
g_quad_cubic + stat_smooth(method = "lm", formula = y ~ poly(x,2), se = F)
  # cubic
g_quad_cubic + stat_smooth(method = "lm", formula = y ~ poly(x,3), se = F)
  # both
(g_quad_cubic 
 + stat_smooth(method = "lm", formula = y ~ poly(x,2), se = F, color = "blue")
 + stat_smooth(method = "lm", formula = y ~ poly(x,3), se = F, color = "green"))


### Dummy variable aproach / Event analysis ###
lm.natcrime_dum <- lm(n.natcrime ~ factor(year), data = sub)
summary(lm.natcrime_dum)

# periodization via dummy variables
sub$period <- with(sub, ifelse(year < 1997, 0, 1))
Tab(sub$period)
lm.natcrime_period <- lm(n.natcrime ~ period, data = sub)
summary(lm.natcrime_period)

sub$XB_period <- lm.natcrime_period$fitted
by.year_period <- ddply(sub, "year", summarise, 
                        NATCRIME = mean(n.natcrime), 
                        XB = mean(XB_period))
by.year_period <- melt(by.year_period, id.vars = "year")
g_period <- ggplot(by.year_period, aes(x = year, y = value, 
                                       group = variable, color = variable))
g_period + geom_point(size = 3) + geom_line()




### Splines ###

# rms package has lsp function for linear splines

# use 1993 as the year separating 2 linear trends 
lm.natcrime_sp1 <- lm(n.natcrime ~ lsp(year, 1993), data = sub)
summary(lm.natcrime_sp1)

# similar to running 2 models (before & after)
lm(n.natcrime ~ year, data = sub, year <= 1993)
lm(n.natcrime ~ year, data = sub, year > 1993)

# graphing it
sub$XB_sp1 <- lm.natcrime_sp1$fitted
by.year_sp1 <- ddply(sub, "year", summarise, NATCRIME = mean(n.natcrime), XB = mean(XB_sp1))
by.year_sp1 <- melt(by.year_sp1, id.vars = "year")
g_sp1 <- ggplot(by.year_sp1, aes(x = year, y = value, group = variable, color = variable))
g_sp1 + geom_line() + geom_point(size = 3)



# splines with more knots
  # using 4 knots (so 5 evenly-spaced time periods)
year.quantiles <- quantile(sub$year, probs=seq(from = 0.2, to = 0.8, by = 0.2))
year.quantiles
lm.natcrime_sp4 <- lm(n.natcrime ~ lsp(year, year.quantiles), data = sub) 
summary(lm.natcrime_sp4)

# sex differences and splines
sub$male <- ifelse(sub$sex == 1, 1, 0)
summary(lm(n.natcrime ~ male*lsp(year, 1993), data = sub))

# see the same thing with two regressions
summary(lm(n.natcrime ~ male*year, data = sub, year < 1994))
summary(lm(n.natcrime ~ male*year, data = sub, year >= 1994))


### Exponential trends ###

# take natural log of dependent variable
sub$ln_n.natcrime <- with(sub, log(n.natcrime))
summary(lm(ln_n.natcrime ~ year, data = sub))





# APC with non-linear trends ----------------------------------------------
# _________________________________________________________________________

# a very simple model
summary(lm(n.natcrime ~ lsp(year, 1993) + factor(cohort.cut) + age, data = sub))

# or reduce the model to just period effects
summary(lm(n.natcrime ~ lsp(year, 1993), data = sub))

# include interactions on post-1993 period and cohort, and post-1993 period
# and age

sub[,c("lsp1993_1", "lsp1993_2")] <- with(sub, lsp(year, 1993))
lm.natcrime_lspInteraction <- lm(n.natcrime ~ lsp1993_1 
                                 + factor(age.cut)*lsp1993_2 
                                 + factor(cohort.cut)*lsp1993_2, 
                                 data = sub)
summary(lm.natcrime_lspInteraction)



# Graphing age*period interactions over time
sub$yhat <- predict(lm.natcrime_lspInteraction)
yhat.by.age <- ddply(sub, c("age.cut", "year"), summarise, yhat = mean(yhat))
legend_options <- theme(legend.position = "bottom", 
                        legend.title = element_text(face = "italic"))
g_yhat.by.age <- ggplot(yhat.by.age, aes(x=year, y=yhat, 
                                         group=age.cut, color=factor(age.cut)))
g_yhat.by.age + geom_line() + legend_options

# Graphing cohort*period interactions over time
yhat.by.cohort <- ddply(sub, c("cohort.cut", "year"), summarise, yhat = mean(yhat))
g_yhat.by.cohort <- ggplot(yhat.by.cohort, aes(x=year, y=yhat, 
                                               group=cohort.cut, color=factor(cohort.cut))) 
g_yhat.by.cohort + geom_line() + legend_options

