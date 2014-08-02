#=====================================#
#====== SOME BASIC STUFF IN R ========#
#=====================================#



# getting help ------------------------------------------------------------
# _________________________________________________________________________

# to see the help file for a particular function use ? or help()
?plot
help(quantile)




# basic arithmetic operations ---------------------------------------------
# _________________________________________________________________________
1 + 99  # addition
1*99  # multiplication
1/99 # division
sqrt(100) # square root
5^3 # exponentiation
log(9) # natural logarithm (base e)
log(9, base = 3) # log base 3
exp(1) # e
exp(2) # e^2


# assignment of values to named objects -----------------------------------
# _________________________________________________________________________

K <- 1 # assign the value 1 to an object named K
K # display the value associated with K

sauce <- 27 # assign the value 27 to an object named sauce

K + sauce # add the values associated with K and sauce

greg <- "greg" # assign the character string "greg" to an object named greg
greg

greg + K # error since greg is a character string and K is a number

K <- 17 # replace the value of K with 17
K

rm(K) # remove the object K 
K # error since K doesn't exist anymore




# using c(), 1:n, seq(), rep() --------------------------------------------
# _________________________________________________________________________

d <- "don't"
w <- "waste"
m <- "my"
t <- "time"
c(d,w,m,t) # c() combines its arguments into a vector


x1 <- c(1,2,3,4,5) 
x1

x2 <- 1:5 # j:k generates a sequence from j to k
x2

x3 <- seq(from = 1, to = 5) # seq(from = j, to = k)  is equivalent to j:k
x3

x4 <- seq(from = 1, to = 5, by = 2) # by = 2 will now make the sequence in steps of 2

# using the length argument instead of by will give an equally spaced sequence
# of the specified length
y <- seq(from = 0, to = 10, length = 7) 
y


# using rep()
z1 <- rep(10,7)
z1

z2 <- rep(z1, 7)
z2




# a few useful vector functions -------------------------------------------
# _________________________________________________________________________

# length (i.e. number of elements)
length(y) 

# sum & product of the elements 
sum(y) 
prod(y) 

# mean & median of the elements
mean(y) 
median(y) 

# min, max & range of the elements
min(y)
max(y)
range(y)

# sorting elements in ascending/descending order
sort(y) 
sort(y, decreasing = TRUE) 

# using which(), which.min(), which.max() to find indices 
which(y == 5) # the index of the element in y that is equal to 5
which(y != 5) # the indices of the element sin y that are not equal to 5
which(y >= 5) # the indices of the elements in y that are greater than or equal to 5
which.min(y) # the index of the smallest element in y
which.max(y) # the index of the largest element in y



# indexing with brackets
y[3] # display the 3rd element in y
y[-3] # display all elements in y except for the 3rd

# applying arithmetic operators to each element 
y + 10 # add 10 to each element
17*y # multiply each element by 17

# element-wise arithmetic with multiple vectors 
(u <- 1:5) # note: using parenthesis like this will not only assign the values to the variable but also display the results
(v <- 5:1) 
u + v 
u - v
u*v



# matrices ----------------------------------------------------------------
# _________________________________________________________________________

# using matrix(), cbind(), rbind(), diag()

A <- matrix(data = c(1, 9, 17, 43), # the data to put in the matrix
            nrow = 2, # use 2 rows
            ncol = 2) # and 2 columns
A

B <- matrix(data = c(1, 9, 17, 43),
            nrow = 2, 
            ncol = 2,
            byrow = TRUE)  # this time fill in the entries by row
B


C <- cbind(c(1,9), c(17,43)) # cbind takes the two vectors and combines them by column
C 

D <- rbind(c(1,9), c(17,43)) # rbind does the same thing but by row 
D

I <- diag(2) # 2x2 identity matrix 
I


# accessing indivual elements, rows, and columns of a matrix

A[1, 1] # display the element in the 1st row and 1st column of A
A[2, 1] # display the element in the 2nd row and 1st column of A
A[1, ] # display the first row of A
A[, 1] # display the first column of A



# matrix operations

A + B # matrix addition

(C <- matrix(-4:4, 3, 3) )
A + C # error because A and C don't have same dimensions

abs(C) # #take absolute value of all elements in C

A*B # NOT normal matrix multiplication (using * just does element-wise multiplication)

A%*%B # matrix multiplication

solve(B) # get the matrix inverse of B

t(C) # get the transpose of C


# setting column and row names
colnames(A) <- c("disturbed", "octopus")
A

rownames(A) <- c("tranquil", "leopard")
A

A[1,1]
A["tranquil", "disturbed"]




# the simplest plots ------------------------------------------------------
# _________________________________________________________________________

# first create some fake data to use
X <- 1:100 
Y <- rnorm(100) # generate 100 random values from standard normal distribution
Z <- sample(1:10, 100, replace = TRUE) # pick 100 integers between 1 and 10 with replacement


# scatter plot
plot(X, Y) 

# histogram
hist(Z)

# barplot 
barplot(Y) # the height of the bars will be the values of Y

# boxplots
boxplot(Y)
boxplot(Z)
boxplot(Y, Z)



# adding to existing plots ------------------------------------------------
# _________________________________________________________________________

# make the scatterplot again
plot(X, Y)

# add a vertical line at X = 50
abline(v = 50)

# add a horizontal line at Y = 0
abline(h = 0)

# add a line with y-intercept = -2 and slope = 1/100 
abline(a = -2, b = 1/100)

# add lines connecting the (X,Y) points
lines(X, Y)

# add a the word "binoculars" at the coordinates (40, 2)
text(40, 2, "binoculars")


# make a new plot, with a histogram of Y
hist(Y, freq = FALSE) # freq = FALSE gives probability densities instead of counts

# add the standard normal distribution curve in blue and with dashed lines
curve(dnorm, add = TRUE, col = "blue", lty = 2) # lty is the argument for linetype

# add a legend in the topright corner
legend("topright", legend = "Normal(0,1)", lty = 2, col = "blue")





# random samples from probability distributions ---------------------------
# _________________________________________________________________________

# for (more or less) each probability distribution that R knows it has functions
# starting with the letters d, p, q and r. Taking the normal distribution as an
# example, R has the functions
#       dnorm(), the probability density function
#       pnorm(), the cumulative density function
#       qnorm(), the quantile function
#       rnorm(), the random number generator

help(Distributions)


# Simulating data for a very simple regression model: 
#   Y_i = alpha + beta*X_i + epsilon_i, where
#     i = 1,...,N and epsilon ~ Normal(mean = 0, sd = sigma)

# let's pick N = 100, a = 5 and b = 2, sigma = 1/3
N <- 100
alpha <- 5
beta <- 2
sigma <- 1/3

# suppose that X is unformly distributed between 20 and 30. we can simulate X
# values using the runif() function
X <- runif(N, min = 20, max = 30)
X

# now we can simulate epsilon using rnorm()
epsilon <- rnorm(N, mean = 0, sd = sigma)

# now we can calculate the Y values
Y <- alpha + beta*X + epsilon
Y


