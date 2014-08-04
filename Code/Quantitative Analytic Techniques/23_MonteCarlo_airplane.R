#======================================================#
#====== MONTE CARLO SIMULATION: AIRPLANE PROBLEM ======#
#======================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014




# THE SCENARIO ------------------------------------------------------------

# An airplane has N seats (N >= 2) and there are N passengers in line waiting to
# board to the plane.

# For simplicity --but without loss of generality-- assume that each passenger
# has a ticket for the seat number corresponding to his/her position in line
# (e.g. the 27th passenger in line has a ticket for seat 27).

# When the first passenger enters the plane, instead of sitting in seat 1
# (his/her assigned seat) he/she picks a seat (uniformly) at random from the N
# seats (so it could be seat 1, but all seats are equally likely).

# The remaining N-1 passengers proceed as follows: if the correct seat is
# available they always take it. However, if the correct seat is not available
# then they pick a seat (uniformly) at random from the available seats.


# THE QUESTION ------------------------------------------------------------

# What is the probability that the last passenger sits in his/her assigned seat?
# In other words, what is the chance that seat N is available when passenger N
# boards the plane?



# SIMULATIONS -------------------------------------------------------------

airplane <- function(N){ 
  # N = number of seats on the plane (and we assume that there are same number
  # of passengers as seats)
  
  seat <- 1:N # numbered seats from 1 to N
  passenger <- rep(0, N) # a vector of 0s to be filled in the with the number of the passenger occupying the corresponding seat
  s <- sample(N, size = 1) # first passenger picks a seat number s uniformly at random from 1 to N
  passenger[s] <- 1 # set element s in the passenger vector to be 1
  
  i <- 2 # now we start with passenger 2
  
  while(i <= N){ # tells R to continue the process below until i == N
    if(passenger[i] != 0) { # if TRUE then passenger i's seat is occupied 
      s <- sample(x = seat[passenger == 0], size = 1) # passenger i pick's a random seat number from the empty seats
      passenger[s] <- i 
    }
    else { # if passenger i's seat is empty then passenger i takes the seat i
      passenger[i] <- i 
    }
    
    i = i + 1 # increment i
  }
  
  LastSeat <- seat[passenger == N] # get the seat number of the last passenger
  return(LastSeat)
}


# run 1000 simulations for a plane with 100 seats (and 100 passengers)
airplane.dat100 <- replicate(10^3, airplane(100))
table(airplane.dat)
mean(airplane.dat100 == 100)

# it turns out that it doesn't matter how many seats the plane has: the last
# passenger always gets either the first seat or the last seat, and with equal
# probability!!
airplane.dat27 <- replicate(10^3, airplane(N = 27))
table(airplane.dat)
mean(airplane.dat27 == 27)


# do replications of size 2^n for n = 1, 2, ..., 15 and plot the proportion of
# times that the last passenger gets the last seat. we'll see that the
# proportion converges to 1/2 as the number of replications gets larger
airplane.dat <- sapply(1:15, FUN = function(n){
  mean(replicate(2^n, airplane(N = 25)) == 25) # we'll use a 25-seat plane, but it doesn't matter
})

plot(NULL, type = "n", xlim = c(1, 15), ylim = c(0, 1), 
     bty = "l", axes = F, xlab = "Number of Replications", ylab = "Proportion", 
     main = "How often does last passenger get last seat?")
lines(1:15, airplane.dat, col = "skyblue", lwd = 2)
points(1:15, airplane.dat, pch = 18, col = "skyblue4")
axis(side = 1, col = "maroon4", las = 2,
     pos = 0.5, at = seq(0,15,5), 
     labels = paste0("2^", seq(0,15,5)))
axis(side = 2, col = "maroon4", las = 2,
     pos = 0.5, at = c(0, 0.5, 1))