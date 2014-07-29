# MARKOV CHAIN

SIMS <- 1000
state <- rep(0, SIMS)
state[1] <- sample(1:4, size = 1)
j = 1
while (j < SIMS) {
  if (state[j] == 1) {
    state[j+1] <- sample(c(2,3,4), size = 1, prob = c(1/3, 1/4, 5/12))
  }
  if (state[j] == 2) {
    state[j+1] <- sample(c(1,3,4), size = 1, prob = c(1/3, 1/3, 1/3)) 
  }
  if(state[j] == 3) {
    state[j+1] <- sample(c(3,4), size = 1, prob = c(1/4, 3/4)) 
  }
  if(state[j] == 4) {
    state[j+1] <- sample(c(2,4), size = 1, prob = c(1/2, 1/2)) 
  }
  points(state[j], j, col = clrs[state[j]], pch = 18)
  j = j+1
}

library(ggplot2)
qplot(state)
plot(ts(state))




