# MARKOV CHAIN

transition.probabilities <- c(0, 1/3, 1/3, 1/3, 
                              1/4, 1/4, 1/4, 1/4,
                              1/4, 0, 1/4, 1/2,
                              1/8, 1/8, 1/8, 5/8)
Q <- matrix(transition.probabilities, 4, 4, byrow = TRUE)
Q
rowSums(Q)

SIMS <- 10^4
state.space <- paste0("State",1:4)
rownames(Q) <- state.space
chain <- rep(0, SIMS)
chain[1] <- sample(state.space, size = 1)
j = 1
while (j < SIMS) {
      chain[j + 1] <- sample(state.space, size = 1, prob = Q[chain[j], ])
      j <- j + 1
}

proportions <- sapply(state.space, function(x) mean(chain == x))
proportions

counts <- sapply(state.space, function(x) sum(chain == x))
counts

library(ggplot2)
g <- ggplot(NULL, aes(x = chain, fill = chain)) + geom_histogram()
g + annotate("text", x = state.space, y = (counts + SIMS/50), label = proportions)




# MORE STATES
LETTERS
state.space2 <- LETTERS
probs <- runif(26^2)
Q2 <- matrix(probs, 26, 26)

for(i in 1:26){
  Q2[i, ] <- Q2[i,]/sum(Q2[i,]) # normalize so rows sum to 1
}

rowSums(Q2)
rownames(Q2) <- state.space2


SIMS2 <- 10^4
chain2 <- rep(0, SIMS)
chain2[1] <- sample(state.space2, size = 1)
j = 1
while (j < SIMS2) {
  chain2[j + 1] <- sample(state.space2, size = 1, prob = Q2[chain2[j], ])
  j <- j + 1
}


proportions2 <- sapply(state.space2, function(x) mean(chain2 == x))
proportions2

counts2 <- sapply(state.space2, function(x) sum(chain2 == x))
counts2

g2 <- ggplot(NULL, aes(x = chain2, fill = chain2)) + geom_histogram()
g2 + theme(legend.position = "none") 

