#=================================#
#====== SIMPLE RANDOM WALKS ======#
#=================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3


# Function to generate N_walks random walks of length N_steps with y[1] = init,
# y[t+1] = y[t] + Normal(0,1)
random_walks <- function(N_walks, N_steps, init = 0) {
  walk <- function(...) {
    y <- c(init, rep(NA, N_steps - 1))
    for(i in 2:N_steps){
      y[i] <- y[i-1] + rnorm(1)
    }
    y
  }
  
  replicate(N_walks, walk(N_steps))
}


# Function to plot the random walks
plot_random_walks <- function(walks) {
  title <- paste("N_walks =", ncol(walks), 
                 " |  N_steps =", nrow(walks), 
                 " |  initial value =", walks[1,1])
  
  plot(ts(walks), plot.type = "single", bty = "l", col = rainbow(ncol(walks)),
       main = "", xlab = title, ylab = "")
  
  mean <- round(mean(walks), 2)
  var <- round(mean(apply(walks, 2, var)), 2)
  var_half <- round(mean(apply(walks[1:nrow(walks)/2,], 2, var)), 2)

  
  mtext(paste0("Avg. Variance at t_", nrow(walks), " : ", var), 
        side = 3, cex = 0.9)
  mtext(paste0("Avg. Variance at t_", nrow(walks)/2, " : ", var_half), 
        side = 3, line = 1, cex = 0.9)
  mtext(paste0("Overall Mean : ", mean), side = 3, line = 2, cex = 0.9)
}


# 2 random walks of length 50, init = 0
walk_sims_2_50 <- random_walks(N_walks = 2, N_steps = 50)
plot_random_walks(walk_sims_2_50)

# 30 random walks, each of length 50, init = 0
walk_sims_30_50 <- random_walks(N_walks = 30, N_steps = 50)
plot_random_walks(walk_sims_30_50)

# 30 random walks, each of length 50, init = 10
walk_sims_30_50_init10 <- random_walks(N_walks = 30, N_steps = 50, init = 10)
plot_random_walks(walk_sims_30_50_init10)

# 15 random walks, each of length 1000, init = 0
walk_sims_15_1000 <- random_walks(N_walks = 15, N_steps = 1000)
plot_random_walks(walk_sims_15_1000)


# Look at how the average value of the walks stays roughly constant while the
# average variance of the walks continues to grow as t increases
plot_mean_var <- function(walks) {
  title <- paste("N_walks =", ncol(walks), 
                 " |  N_steps =", nrow(walks), 
                 " |  initial value =", walks[1,1])
  
  mean <- walks[1,1]
  var <- 0
  for (j in 2:nrow(walks)) {
    mean <- c(mean, mean(walks[1:j, ]))
    var <- c(var, mean(apply(walks[1:j, ], 2, var)))
  }
  line_cols <- c("red3", "blue3")
  plot(ts(cbind(var, mean)), plot.type = "single", bty = "l",
       ylim = c(min(mean, var), max(mean, var) + 3),
       col = line_cols, ylab = "", xlab = title)
  
  txt_x <- nrow(walks)/2
  text(x = txt_x, y = c(var[txt_x], mean[txt_x]), pos = c(4,3), col = line_cols,
       labels = c("Avg. Variance", "Avg. Value"))
}

plot_mean_var(walk_sims_30_50_init10)
plot_mean_var(walk_sims_15_1000)


# Look at plot of walks and plot of mean/variance side by side
par(mfrow = c(2,1))
plot_random_walks(walk_sims_15_1000)
plot_mean_var(walk_sims_15_1000)
