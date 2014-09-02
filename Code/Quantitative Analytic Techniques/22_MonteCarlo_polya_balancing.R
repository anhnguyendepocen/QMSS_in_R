#=================================================================#
#====== MONTE CARLO SIMULATION: POLYA & BALANCING PROCESSES ======#
#=================================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3


# Suppose we have an urn containing one red ball and white ball. Consider two
# different processes: 

  # 1. Polya process: pick a ball from the urn, replace it and add another ball
  # of the same color to the urn. Repeat. 

  # 2. Balancing process: pick a ball from the urn, replace it and add another
  # ball of the other color to the urn. Repeat. 

# It turns out that in the Polya process any long-run proportion of red balls in
# the urn is equally likely, whereas in the Balancing process the long-run 
# proportion of reds is 1/2. Both the Polya and Balancing processes have have an
# _expected_ proportion of heads of 1/2.


# POLYA PROCESS -----------------------------------------------------------
polya <- function(draws){
  red <- c(TRUE, FALSE)
  i = 1
  while(i <= draws){
    draw <- sample(red, 1)
    if(draw == TRUE) {
      red <- c(red, TRUE)
    }
    else {
      red <- c(red, FALSE)
    }
    i = i + 1
  }
  red
}


# BALANCING PROCESS -------------------------------------------------------
balancing <- function(draws){
  red <- c(TRUE, FALSE)
  i = 1
  while(i <= draws){
    draw <- sample(red, 1)
    if(draw == TRUE) {
      red <- c(red, FALSE)
    }
    else {
      red <- c(red, TRUE)
    }
    i = i + 1
  }
  red
}



# SIMULATIONS -------------------------------------------------------------

# set number of draws
N <- 10^3

# simulations
polya.dat <- replicate(100, polya(10^3))
balancing.dat <- replicate(100, balancing(10^3))
polya.prop.red <- apply(X = polya.dat, MARGIN = 2, FUN = mean)
balancing.prop.red <- apply(X = balancing.dat, MARGIN = 2, FUN = mean)

mean(polya.prop.red)
mean(balancing.prop.red)

# plots
library(ggplot2)
g <- ggplot(NULL, aes(x = 1:100, y = polya.prop.red)) + labs(list(x = "Simulation", y = "Proportion"))
g <- g + geom_line(color = "pink3") + geom_line(aes(y = balancing.prop.red), color = "darkred")
g

line.labs <-   annotate("text", label = c("Polya", "Balancing"), 
                   x = 50, y = c(0.9, 0.6), size = 10, 
                   colour = c("pink3", "darkred"))

mean.labs1 <- paste("mean =", round(mean(polya.prop.red), 3))
mean.labs2 <- paste("mean =",round(mean(balancing.prop.red), 3))
mean.labs <- annotate("text", label = c(mean.labs1, mean.labs2), color = c("pink3", "darkred"),
                      x = c(30, 70), y = 0.0, size = 6)

g + line.labs + mean.labs
  