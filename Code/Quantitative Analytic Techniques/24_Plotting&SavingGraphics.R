#=============================================================#
#====== CREATING & SAVING PLOTS USING FUNCTIONS & LOOPS ======#
#=============================================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/19/2014


# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

library(QMSS)
library(RColorBrewer)


# Generate some fake data to use as an example
age <- sample(20:50, size = 1000, replace = T)
score <- rep(NA, 1000)
score[age < 30] <- rnorm(sum(age < 30), mean = 90, sd = 10)  
score[age %in% 30:40] <- rnorm(sum(age %in% 30:40), mean = 80, sd = 15)
score[age > 40] <- rnorm(sum(age > 40), mean = 70, sd = 20)


# Suppose we want to make a histogram of scores in a given age range
hist(score[age < 25], col = "darkgray", border = "white",
     xlab = "Score", main = "Scores for age < 25")
hist(score[age %in% 20:30], col = "darkgray", border = "white",
     xlab = "Score", main = "Scores for ages 20 to 30")
# etc.

# But what if we don't want to type or cut/paste all of that everytime we want to add a plot
# for a different age range?

# To make things a bit easier to read and save a lot of space in the code we can first make our 
# plotting command into a new function that takes a min and max (defining a range of ages) as its argument
plotByAge <- function(minAGE, maxAGE, ...){ # including "..." allows us to later pass other arguments, e.g. colors, to hist() 
  hist(score[age %in% minAGE:maxAGE], 
       xlab = "Score",
       main = paste("Scores for ages", minAGE, "to", maxAGE),
       ...) # include "..." again here 
}

# try out our function
plotByAge(27, 33)
plotByAge(31, 40, col = "skyblue", border = "white")
plotByAge(30, 49, col = "maroon", border = "darkgray")


# Here's an example of stacking 3 histograms (each with a different age range) and saving the combined plot 
# First we specify the type of file and the name we want to use. We'll use pdf, so we call the pdf function 
# which initiates the graphics device driver for producing pdfs (see ?pdf for customizable options)
pdf(file = "ageplots.pdf") # for other formats there are the analagous functions, e.g. jpeg(), svg(), png(), tiff(), etc
par(mfrow = c(3,1)) # to put 3 graphs one on top of the other
colors <- brewer.pal(3, "Dark2") # some colors to use for the histograms
min <- c(20, 30, 36) # mins for the age ranges
max <- c(29, 35, 40) # maxs for the age ranges
for(i in 1:3){  
  plotByAge(min[i], max[i], border = "white", col = colors[i]) # use our newly created function
}
dev.off() # shut down the graphics device (this is IMPORTANT!!! )
# now there should be a file called "ageplots.pdf" saved in your working directory


# Suppose instead of stacking the 3 plots we wanted a different file for each of them
# We bring the pdf creation inside our loop 
for(i in 1:3){  
  plot.name <- paste0("ageplot",min[i],"_",max[i],".pdf")  # name for the plot (e.g. first will be "ageplot20_29.pdf")
  pdf(file = plot.name) 
  plotByAge(min[i], max[i], border = "white", col = colors[i])
  dev.off() # also need to bring dev.off() inside the loop so the pdf() command will start a new plot each time 
}
# After running the loop there should be three new plots in the working directory:
#   "ageplot20_29.pdf",   "ageplot30_35.pdf",   "ageplot36_40.pdf"


# Suppose we don't want to save the plots directly to the working directory but rather to 
# a folder called "Plots" within the working directory. We can manually create the folder 
# and then just change the file name we give to pdf() to "Plots/interaction_plot.pdf", 
# or we can have R create the "Plots" folder for us. This can be very useful when writing code
# that generates all sorts of different plots or tables that we want to be saved in different
# directories or subdirectories. 

# Create "Plots" directory (i.e. folder) inside the working directory
dir.create("Plots")
# there should now be a folder called Plots in your working directory

# If we want, we can now create subdirectories inside of "Plots" 
# Maybe we want to save each plot in three file types (png, svg, pdf) so we can choose which 
# to use later. We can create the subdirectories "PDF", "SVG" and "PNG" to organize them 
sub.dirs <- c("PDF", "JPEG", "PNG")
for(dir in sub.dirs){
  dir.create( paste0("Plots/", dir) )
}
# there should now be the folders PDF, PNG, and SVG inside the Plots directory

# HOWEVER, if we come back and run the entire code again later R will try to create the directories
# again and we will get errors because they already exist. There are a few ways to avoid this. 
# We could add the argument "showWarnings = FALSE" to dir.create() in the loop above, but this could 
# suppress other types of warnings that might be important. Instead we can just write a better loop 
# that checks if the directories exist and only creates them if they don't 

if(!file.exists("Plots")) {
  dir.create("Plots") 
}
sub.dirs <- c("PDF", "PNG", "SVG")
for(dir in sub.dirs){
  dir.name <- paste0("Plots/", dir)
  if(!file.exists(dir.name)) dir.create(dir.name)
}

# Suppose we want to create histograms for many more age ranges AND we want to create  
# pdf, png, and svg versions of each plot and save them in the appropriate subdirectory 
# in "Plots"

min <- seq(20, 45, by = 5) # sequence from 20 to 45 in increments of 5
max <- seq(25, 50, by = 5) # sequence from 25 to 50 in increments of 5
N <- length(min)
colors <- brewer.pal(N, "Dark2")

file.types <- c("pdf", "png", "svg")
for(i in 1:N){ # loop over the indices of the min, max and colors vectors
  for(TYPE in file.types){ # loop over the file types
    plot.name <- paste0("Plots/", TYPE, "/ageplot",min[i],"_",max[i],".", TYPE) # e.g. i = 1 and TYPE = "pdf" this line will be equivalent to: plot.name <- "Plots/pdf/ageplot20_25.pdf"
    do.call(TYPE, args = list(file = plot.name)) # do.call() is a way of executing a function from it's name as a character string. the functions we want are already character strings in the file.types vector
    plotByAge(min[i], max[i], border = "white", col = colors[i]) # use our plotByAge function to make the plot
    dev.off()
  }
}  

# After running this loop the should be jpeg, pdf, png plots for each of the age ranges in the 
# in the appropriate subdirectory of Plots
