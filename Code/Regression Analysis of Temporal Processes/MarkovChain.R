# MARKOV CHAIN

transition.probabilities <- c(0, 1/3, 1/4, 5/12, 
                              1/4, 1/4, 1/4, 1/4,
                              0, 0, 1/4, 3/4,
                              1/8, 1/8, 1/8, 5/8)
Q <- matrix(transition.probabilities, 4, 4, byrow = TRUE)
Q
rowSums(Q)

SIMS <- 100
state.space <- 1:4
chain <- rep(0, SIMS)
chain[1] <- sample(state.space, size = 1)
j = 1
while (j < SIMS) {
      chain[j + 1] <- sample(state.space, size = 1, prob = Q[chain[j], ])
      j <- j + 1
}


for(i in state.space){
  prop <- mean(chain == i)
  print(paste("Long-run proportion: State",i,"=", prop))
}



plot(NULL, xlim = c(1,4), ylim = c(0,SIMS), ylab = "", xlab = "", axes = F)
for(i in 1:SIMS){
  points(chain[i], i, col = colors()[sample(SIMS, 1)], pch = 19)
  Sys.sleep(.5)
}





