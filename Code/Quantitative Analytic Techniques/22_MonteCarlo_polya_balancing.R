#=================================================================#
#====== MONTE CARLO SIMULATION: POLYA & BALANCING PROCESSES ======#
#=================================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014




# In the Polya process any proportion of reds is equally likely
# In the Balancing process the long-run proportion of reds is 1/2

# They both have an _expected_ proportion of heads of 1/2 


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
plot(polya.prop.red, type = "n",  bty = "l", 
     xlab = "simulation", ylab = "proportion of reds",
     sub = paste("number of draws =", N), cex.sub = 0.8)
lines(polya.prop.red, lwd = 1.5, col = "pink3") 
lines(balancing.prop.red, lwd = 1.5, col = "darkred")
legend("top", c("Polya", "Balancing"), ncol = 1, bg = "lightgray",
       col = c("pink3", "darkred"), lwd = 2, seg.len = 1)

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
  