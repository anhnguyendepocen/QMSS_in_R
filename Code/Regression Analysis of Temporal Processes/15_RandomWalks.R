#=================================#
#====== SIMPLE RANDOM WALKS ======#
#=================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3

library(ggplot2)
library(reshape2)


# Function to generate N_walks random walks of length N_steps with y[1] = init,
# y[t+1] = y[t] + Normal(0,1)
random_walks <- function(N_walks, N_steps, init = 0) {
  walk <- function() {
    y <- c(init, rep(NA, N_steps - 1))
    for(i in 2:N_steps){
      y[i] <- y[i-1] + rnorm(1)
    }
    y
  }
  
  replicate(N_walks, walk(N_steps))
}


# Function to plot the random walks
gg_random_walks <- function(walks) {
  require(ggplot2)
  require(reshape2)
  
  molten.walks <- melt(walks, varnames = c("t", "Walk"))
  gg_walks <- ggplot(molten.walks, aes(x = t, y = value, group = Walk, color = factor(Walk)))
  
  # make plot title
  title <- paste("N_walks =", ncol(walks), 
                 " |  N_steps =", nrow(walks), 
                 " |  initial value =", walks[1,1])
  
  # display overall average value and average value at last step 
  mean <- round(mean(walks), 2)
  mean_lab1 <- paste("Overall Avg. Value =", mean)
  mean_end <- round(mean(walks[nrow(walks), ]), 2)
  mean_lab2 <- paste("Avg. Value at Final Step =", mean_end)
  
  t_range <- range(molten.walks$t)
  mean_labs <- annotate("text", 
                        x = t_range + t_range[2]*c(1/5,-1/5), 
                        y = max(walks) + 10, 
                        label = c(mean_lab1, mean_lab2))
  
  gg_out <- (gg_walks + geom_line() + theme(legend.position = "none") 
             + ggtitle(title) + mean_labs)
  gg_out
}


# 1 random walk of length 50, init = 0
walk_sims_1_50 <- random_walks(N_walks = 1, N_steps = 50)
gg_random_walks(walk_sims_1_50)

# 30 random walks, each of length 50, init = 0
walk_sims_30_50 <- random_walks(N_walks = 30, N_steps = 50)
gg_random_walks(walk_sims_30_50)

# 30 random walks, each of length 50, init = 10
walk_sims_30_50_init10 <- random_walks(N_walks = 30, N_steps = 50, init = 10)
gg_random_walks(walk_sims_30_50_init10)

# 15 random walks, each of length 1000, init = 0
walk_sims_15_1000 <- random_walks(N_walks = 15, N_steps = 1000)
gg_random_walks(walk_sims_15_1000)

