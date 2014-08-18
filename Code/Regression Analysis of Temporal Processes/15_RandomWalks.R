#=================================#
#====== SIMPLE RANDOM WALKS ======#
#=================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3

library(ggplot2)
library(reshape2)

random_walk <- function(N) {
  y <- rep(0, N)
  for(i in 2:N){
    y[i] <- y[i-1] + rnorm(1)
  }
  y
}


gg_random_walks <- function(walks) {
  molten.walks <- melt(walks, varnames = c("t", "Walk"))
  gg_walks <- ggplot(molten.walks, aes(x = t, y = value, group = Walk, color = factor(Walk)))
  
  title <- paste(ncol(walks), "Random Walks of Length", nrow(walks))
  title <- ggtitle(title)
  subtitle <- annotate("text", x = nrow(walks)/2, y = max(molten.walks$value) + 10, 
                       label = c("y[1] = 0,  y[t+1] = y[t] + N(0,1)"))
  no_legend <- theme(legend.position = "none")
  
  gg_walks + geom_line() + no_legend + title + subtitle
}


walk_sims <- replicate(20, random_walk(50))
gg_random_walks(walk_sims)

walk_sims2 <- replicate(10, random_walk(1000))
gg_random_walks(walk_sims2)

