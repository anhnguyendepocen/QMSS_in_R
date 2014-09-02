#====================================================#
#====== INTRO TO MAXIMUM LIKELIHOOD ESTIMATION ======#
#====================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3



# Simple coin tossing example ---------------------------------------------
# _________________________________________________________________________

# choose number of tosses N
n.tosses <- 100

# simulate coin flips using sample() 
coin.tosses <- sample(x = c("Heads","Tails"), # possible outcomes
                      size = n.tosses, # number to choose
                      replace = T, # sample from x with replacement  
                      prob = c(1/2, 1/2)) # probability weights for the possible outcomes
table(coin.tosses)

# the likelihood function
likelihood <- function(p){ # argument is p, so for different values of p we get different likelihoods  
  n.heads <- sum(coin.tosses == "Heads") # number of heads observed
  
  dbinom(x = n.heads,     # binomial probability mass function for n.heads successes 
         size = n.tosses, # in n.tosses trials
         prob = p)        # as a function of the probability of success/heads, p
}

# note: instead of using the dbinom function we could also have typed out the binomial probability
#   mass function as: choose(n.tosses, n.heads) * p^n.heads * (1-p)^(n.tosses - n.heads)


# find the value of the likelihood function for some different values of p
likelihood(1/2) # likelihood for p = 1/2
likelihood(1/3) # likelihood for p = 1/3

# look at distribution of the likelihood function for p given our coin.tosses data
curve(likelihood, col = "turquoise4", lwd = 2, bty="l")

# we know that the maximum likelihood estimate (MLE) in this simple example is just the observed
#   proportion of heads, but in general we can have R maximize a likelihood function using optimize() 
optimize(likelihood, # function to optimize
         interval = c(0,1), # interval to be searched
         maximum = T) # compute max instead of min

# the output from the optimize function has two components:
#   "maximum" is the value of p that maximizes the likelihood function
#   "objective" is the value of the likelihood function at the maximum
#   we can extract just "maximum" using the $ operator
max <- optimize(likelihood, interval = c(0,1), maximum = T)$maximum 

# show the maximum likelihood estimate and the true probability on our plotted likelihood curve
points(max, likelihood(max), pch = 19, col = "purple4")
points(1/2, likelihood(1/2), pch = 19, col = "blue")
text(max, likelihood(max), "MLE", pos = 2, col = "purple4")
text(1/2, likelihood(1/2), "True p", pos = 4, col = "blue")
txt <- c(paste("MLE = ", round(max,3)," | True p = ", 1/2), paste("Error = ", abs(1/2 - round(max,3))))
title(main = txt, cex.main = 0.9)




# Likelihood functions for different numbers of heads ---------------------
# _________________________________________________________________________

library(RColorBrewer)
colors <- brewer.pal(6, "Set1")
lik <- function(p) dbinom(n.heads, n.tosses, p)
n.tosses = 100
n.heads = 5
curve(lik, lwd = 2, col = colors[1], xlab = "p", ylab = "likelihood", bty = "l")
heads = c(10, 25, 50, 75, 95)
for(i in 1:length(heads)){
  n.heads = heads[i]
  curve(lik, lwd = 2, col = colors[i+1], add = T)
}

legend("top", legend = c(5, heads), title = "Number of heads in 100 tosses",
       ncol = 3, bty = "n", lwd = 2, seg.len = 1, col = colors)



# Extending the coin tossing example --------------------------------------
# _________________________________________________________________________

# In the coin toss example above we set it so that the coin was fair. But we can also set it so
# that we don't know what the probability of heads is in advance. We can pick the probability
# at random and then see what the maximum likelihood estimate is

# set probability of heads 
p_heads <- runif(1) # runif(1) will pick 1 number uniformly at random between 0 and 1
# get a new set of coin tosses
coin.tosses <- sample(x = c("Heads","Tails"), 
                      size = n.tosses, 
                      replace = T, 
                      prob = c(p_heads, 1 - p_heads)) # use the randomly chosen p_heads
table(coin.tosses)
# take a look at the new likelihood function
curve(likelihood, col = "turquoise4", lwd = 2, bty="l")
# get the MLE
max <- optimize(likelihood, interval = c(0,1), maximum = T)$maximum 
# show the MLE and the true probability on our plotted likelihood curve
points(max, likelihood(max), pch = 19, col = "purple4")
points(p_heads, likelihood(p_heads), pch = 19, col = "blue")
text(max, likelihood(max), "MLE", pos = 2, col = "purple4")
text(p_heads, likelihood(p_heads), "True p", pos = 4, col = "blue")
txt <- c(paste("MLE = ", round(max,3)," | True p = ", round(p_heads, 3)), 
         paste("Error = ", abs(round(p_heads - max, 3)))
         )
title(main = txt, cex.main = 0.9)



### The difference between the MLE and the true p should shrink as n.tosses gets larger ###

coinsMLE <- function(n.tosses, p){ # n.tosses = number of tosses, p = P(Heads). 
  tosses <- sample(x = c("Heads","Tails"), 
                   size = n.tosses, 
                   replace = T, 
                   prob = c(p, 1 - p)) 
  # use the log-likelihood function (the natural logarithm of the likelihood) 
  #   using the log reduces the potentital for underflow/overflow when the computer tries to calculate
  #   the value that maximize the log-likelihood is the same value that maximizes the likelihood so the
  #   estimate is the same (i.e. argmax{log-likelihood} = argmax{likelihood})
  loglik <- function(x) dbinom(sum(tosses == "Heads"), size = n.tosses, prob = x, log = TRUE)
  MLE <- optimize(loglik, interval = c(0,1), maximum = T)$maximum # maximize the log-likelihood
  err <- abs(p - MLE) # compute magnitude of the error (i.e. how far off is the estimate?)
  err
}

plot.coinsMLE <- function(sims, N, p, add = FALSE, ...){ # N will be a vector of different numbers of tosses to try
  dat <- replicate(n = sims, # number of times to do the experiment
                   expr = sapply(N, FUN = function(k) coinsMLE(n.tosses = k, p)))
  dat <- rowMeans(dat)
  plot(x = N, y = dat,
       xlab = "Number of tosses",
       ylab = "Mean error",
       main = paste(sims, "replications"),
       ...)
}

plot.coinsMLE(sims = 20, 
              N = c(10, seq(from = 100, to = 2*10^4, by = 500)), 
              p = 1/2,
              type = "l", lwd = 2, col = "turquoise4") 



### we should also see less variability as we increase the number of replications ###

par(mfrow = c(2,2))
reps <- c(5, 20, 50)
colors <- rainbow(4)
plot.coinsMLE(sims = 1, 
              N = c(10, seq(from = 100, to = 5*10^4, by = 500)), 
              p = 1/2,
              type = "h", lwd = 1, col = colors[1]) 

for (i in 1:3) {
  plot.coinsMLE(sims = reps[i], 
                N = c(10, seq(from = 100, to = 5*10^4, by = 500)), 
                p = 1/2,
                type = "h", lwd = 1, col = colors[i+1]) 
}