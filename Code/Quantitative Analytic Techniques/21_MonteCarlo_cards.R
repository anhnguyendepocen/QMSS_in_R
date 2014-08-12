#==================================================#
#====== MONTE CARLO SIMULATION: COIN TOSSING ======#
#==================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014



# Suppose we have two sets of 10 colored cards. The first set of cards has 6 red
# cards and 4 blue cards. The second set of cards has 8 red cards and 2 blue
# cards.

# Without looking at the color, we draw one card from the first set and place it
# in the second set. (So the first set now has 9 cards and the second has 11
# cards)

# Then -- again without seeing the color -- we draw one card from the second set
# and place it in the first set.

# If we were to then take one card from the first set, what is the probability
# that it's a red card?



# Strategy: 
#   1. write a function that simulates this experiment with the cards
#   2. run the experiment many times
#   3. look at the proportion of the cards we end up with that are red cards 


cardsExperiment <- function(){
# SETUP THE SETS OF CARDS
  set_1 <- c(rep("RED", 6), rep("BLUE", 4))
  set_2 <- c(rep("RED", 8), rep("BLUE", 2))
  
# PICK A CARD FROM set_1 AND PUT IT IN set_2
#   We'll do this by picking an integer j between 1 and 10 at random and then
#   taking the jth card from set_1 and putting it in set_2
  j <- sample(x = 1:10, size = 1) # pick j
  set_2 <- c(set_2, set_1[j])     # add jth card from set_1 to set_2
  set_1 <- set_1[-j]              # remove jth card from set_1

# PICK A CARD FROM THE NEW set_2 AND PUT IT IN set_1
k <- sample(x = 1:11, size = 1) # pick an integer k between 1 and 11 since set_2 now has 11 cards
set_1 <- c(set_1, set_2[k])     # add kth card from set_2 to set_1 (we don't need to remove it from 
                                # set_2 since we're only interested in now picking a card from set_1)

# PICK THE FINAL CARD FROM set_1
sample(set_1, 1)
}

# Try out the function
cardsExperiment() # running this should output "BLUE" or "RED"

# Now run the experiment 10,000 times using the replicate() function
replicate(10^5, cardsExperiment() )

# Run it again but this time just get the proportion results that are "RED"
# instead of uselessly printing the all of the outcomes
mean( replicate(10^4, cardsExperiment() == "RED") )

# Let's look at how the estimated probabilities converge as the number of
# replications gets larger
estimates <- sapply(0:15, FUN = function(i){
  mean(replicate(2^i, cardsExperiment() == "RED"))
})

library(ggplot2)
xlab <- xlab("Number of replications (in powers of 2)")
ylab <- ylab("Estimated probability")
q <- qplot(x = 0:15, y = estimates, geom = c("line", "point"))
q + xlab + ylab


# We can also modify our function to allow the user to pick the size of the
# decks and the number of red/blue cards in each.
cardsExperiment <- function(n, r1, r2){ 
  # n = number of cards per set, 
  # r1 & r2 = number of reds in set_1 & set_2, respectively 
  
  stopifnot(n >= r1, n >= r2) # give error message if r1 or r2 is greater than # of cards 
  
  # SETUP THE SETS OF CARDS
  set_1 <- c(rep("RED", r1), rep("BLUE", n - r1))
  set_2 <- c(rep("RED", r2), rep("BLUE", n - r2))
  
  # PICK A CARD FROM set_1 AND PUT IT IN set_2
  j <- sample(x = 1:n, size = 1)  # pick j from between 1 and n
  set_2 <- c(set_2, set_1[j])     
  set_1 <- set_1[-j]              
  
  # PICK A CARD FROM THE NEW set_2 AND PUT IT IN set_1
  k <- sample(x = 1:(n+1), size = 1) # pick k from between 1 and n+1
  set_1 <- c(set_1, set_2[k])    
  
  # PICK THE FINAL CARD FROM set_1
  sample(set_1, 1)
}

# Use the new function to estimate the desired probability if the decks start
# with 20 cards and 5 and 16 red cards, respectively
mean( replicate(10^4, cardsExperiment(n = 20, r1 = 5, r2 = 16) == "RED") )

# start with 50 cards each and both with 25 of each color
mean( replicate(10^4, cardsExperiment(n = 50, r1 = 25, r2 = 25) == "RED") )