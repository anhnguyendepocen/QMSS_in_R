# To-do:
  # make ggplots commands less wordy



#===========================================================#
#====== DIFFERENCE-IN-DIFFERENCES, NON-LINEAR TRENDS  ======#
#===========================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/20/2014


# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# Set working directory
setwd("/Users/jgabry/Desktop/COLUMBIA/Stuff_for_Greg/Data")

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

vars <- c("happy", "partyid", "year", "age", "educ", "sex", "realinc", "polviews", "race", "region")
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
# For years 2006 to 2010
library(visreg)
visreg(lm(n.happy~year*repub, data=sub, year %in% 2006:2010), "year", by="repub", 
       band=F, overlay=T, partial=F, line=list(col=c("blue","red")),)

# For all available years
by.year.party <- ddply(sub, .(year), summarise, 
                 mean.dems = mean(n.happy[repub==0], na.rm=T),
                 mean.repubs = mean(n.happy[repub==1], na.rm=T))
print(by.year.party, digits = 3)

with(by.year.party, plot(year, mean.dems, bty = "l", type = "l", lwd = 2, col = "blue",
                   ylim = c(2,2.5), xlab = "Year", ylab = "Mean happiness score", xaxt = "n"))
axis(side = 1, at = seq(1970, 2015, 5), labels = T, las = 2)
lines(by.year.party$year, by.year.party$mean.repubs, col = "red", lwd = 2)
legend("top", c("Republicans", "Democrats"), lwd = 1, col = c("red", "blue"), bty="n", ncol=2)





# Non-Linear Time Trends --------------------------------------------------
# _________________________________________________________________________

# Load our saved APC work
load("APC.RData")

# overall natcrime trend
g_year
(g_year <- g_year + ylim(2.45, 2.75) 
 + theme(panel.background = element_rect(fill = "gray49", colour = 'white'))) 

# add fitted line
(g_year <- g_year + stat_smooth(method = lm, se = FALSE, color = "maroon", lty = 2, lwd = 1.25))

# or just view points and fitted line
(ggplot(by.year, aes(x = year, y = NATCRIME)) + geom_point(color = "skyblue", size = 3) 
 + stat_smooth(method = lm, se = FALSE, color = "maroon", lty = 2, lwd = 1.25)
 + theme(panel.background = element_rect(fill = "gray49", colour = 'white'))
 + ylim(2.45, 2.75) )
 

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
b_quad <- coef(lm.natcrime_quad)
b_cub <- coef(lm.natcrime_cub)
plot(by.year, pch = 20, col = "cyan3")
curve(b_quad%*%rbind(1,x,x^2), add = TRUE, lwd = 2, col = "navyblue") # quadratic
curve(b_cub%*%rbind(1,x,x^2,x^3), add = TRUE, lwd = 2, col = "orangered") # cubic



### Dummy variable aproach / Event analysis ###
lm.natcrime_dum <- lm(n.natcrime ~ factor(year), data = sub)
summary(lm.natcrime_dum)

# periodization via dummy variables
sub$period <- with(sub, ifelse(year < 1997, 0, 1))
Tab(sub$period)
lm.natcrime_period <- lm(n.natcrime ~ period, data = sub)
summary(lm.natcrime_period)

sub$XB_period <- lm.natcrime_period$fitted
by.year_period <- ddply(sub, "year", summarise, NATCRIME = mean(n.natcrime), XB = mean(XB_period))
g_period <- (ggplot(by.year_period, aes(x = year, y = XB)) 
            + geom_point(color = "navyblue", size = 3) + geom_line(color = "navyblue"))
g_period <- (g_period + geom_point(aes(x = year, y = NATCRIME), color = "maroon", size = 3) 
             + geom_line(aes(x = year, y = NATCRIME), color = "maroon"))
g_period + ylab("")



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
plot(by.year, pch = 19, col = "maroon", ylab = "", bty = "l")
points(sub[,c("year","XB_sp1")], pch = 20, col = "navyblue")
lines(sub[,c("year","XB_sp1")], lwd = 2, col = "navyblue")


by.year_sp1 <- ddply(sub, "year", summarise, NATCRIME = mean(n.natcrime), XB = mean(XB_sp1))
g_sp1 <- (ggplot(by.year_sp1, aes(x = year, y = NATCRIME)) 
             + geom_line(color = "gray50") + geom_point(color = "hotpink", size = 3))
g_sp1 <- (g_sp1  + geom_line(aes(x = year, y = XB), color = "gray10")
          + geom_point(aes(x = year, y = XB), color = "cyan3", size = 3))           
g_sp1 + ylab("") 


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

# include interactions on post-1993 period and cohort, and post-1993 period and age

sub[,c("lsp1993_1", "lsp1993_2")] <- with(sub, lsp(year, 1993))
lm.natcrime_lspInteraction <- lm(n.natcrime ~ lsp1993_1 
                                 + factor(age.cut)*lsp1993_2 
                                 + factor(cohort.cut)*lsp1993_2, 
                                 data = sub)
summary(lm.natcrime_lspInteraction)



# Graphing age*period interactions over time
sub$yhat <- predict(lm.natcrime_lspInteraction)
yhat.by.age <- ddply(sub, c("age.cut", "year"), summarise, yhat = mean(yhat))

g_yhat.by.age <- (ggplot(yhat.by.age, aes(x=year, y=yhat, group=age.cut)) 
                  + geom_line(aes(color=factor(age.cut)), lwd = 1.1) 
                  + scale_color_discrete(name = "Age Cuts"))
g_yhat.by.age
g_yhat.by.age + theme(legend.position = "bottom", 
                       panel.background = element_rect(fill = "gray40"), 
                       legend.title = element_text(face = "italic"))
 
# Graphing cohort*period interactions over time
yhat.by.cohort <- ddply(sub, c("cohort.cut", "year"), summarise, yhat = mean(yhat))
g_yhat.by.cohort <- (ggplot(yhat.by.cohort, aes(x=year, y=yhat, group=cohort.cut)) 
                     + geom_line(aes(color=factor(cohort.cut)), lwd = 1.1) 
                     + scale_color_discrete(name = "Cohort Cuts"))
g_yhat.by.cohort 
g_yhat.by.cohort + theme(legend.position = "bottom", 
                         panel.background = element_rect(fill = "gray40"), 
                         legend.title = element_text(face = "italic"))

