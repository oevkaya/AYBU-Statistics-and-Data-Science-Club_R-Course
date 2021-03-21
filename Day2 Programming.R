
# Recall ------------------------------------------------------------------
library(help = "stats")

df <- data.frame('a' = c(1,2,3,4,5) , 'b' = c(11,12,13,14,15))
df
is.data.frame(df)

# Adding vector and matrix together as list
m=matrix(1:10, 5 ,2)
m
l4 <- list('Vector' = c(10,11,12,13) , 'Matrix' = m)
l4

l5 <- list('Vector' = runif(5) , 'Matrix' = m, 'DF' = df)
l5

# %in% usage
x <- rep(1:2, 3)
x
x <- rep(1:2, each=3)
x
x %in% c(1, 5)
x[x %in% c(1, 5)]

# ! x
# x & y
# x && y
# x | y
# x || y

x=c(1,2,3,4,5)
y=c(10,11,12,13,15)

# NOTE 
# & and && indicate logical AND and | and || indicate logical OR. 
# The shorter form performs elementwise comparisons in much the 
# same way as arithmetic operators. 
# The longer form evaluates left to right examining only 
# the first element of each vector. 
# Evaluation proceeds only until the result is determined. 
# The longer form is appropriate for programming control-flow.

#About Programming ---------------
# Commonly used control structures are

# if and else: testing a condition and acting on it
# 
# for: execute a loop a fixed number of times
# while: execute a loop while a condition is true
# 
# break: break the execution of a loop
# next: skip an interation of a loop
# repeat: execute an infinite loop (must break out of it to stop)

# If statement ------------------------------------------------------------
# 
# #if (condition){
#   Do something
# } else {
#   Do something different
# }

x=5
if (x > 4) {
  print("x is greater than 4")
}


i <- 2
if (i > 3){
  # set.seed(11)
  runif(5, 10, 20)
} else {
  message("No output")
}

runif(3)
# if (condition1) { 
#   expr1
# } else if (condition2) {
#   expr2
# } else if  (condition3) {
#   expr3
# } else {
#   expr4
# }

catvec <- LETTERS[1:10]
sample()
#category <- 'A' #  
category <- sample(catvec, 1) 
price <- 10


# cat usage

if (category == 'A'){
  cat('A vat rate of 8% is applied.','The total price is', price *1.08)  
} else if (category == 'B'){
  cat('A vat rate of 10% is applied.','The total price is', price *1.10)  
} else {
  cat('A vat rate of 20% is applied.','The total price is', price *1.20)  
}

# paste('A vat rate of 20% is applied.','The total price is', price *1.20)  

# switch  -----------------------------------------------------------------

x_option <- function(x) {
  if (x == "a") {
    "option 1"
  } else if (x == "b") {
    "option 2" 
  } else if (x == "c") {
    "option 3"
  } else {
    "option 4"
    # stop("Invalid `x` value")
  }
}

x_option(q)

# In a more short way we can use switch 

x_option <- function(x) {
  switch(x,
         a = "option 1",
         b = "option 2",
         c = "option 3",
         d = "option 4"
  ) }

x_option("a")

# ifelse ------------------------------------------------------------------
mydata=sample(100, 10)
mydata
class(mydata)

class(df[1])

x = ifelse(df$a>3, 1, df$a)
x


ifelse(mydata > 50, 50, mydata)


newdata = cbind(x,mydata)

# For Loop ----------------------------------------------------------------

# for (variable in sequence){
#   Do something
# }

# Example 
1:4
for (i in 1:4){
  j <- i + 10
  toget_ij <- cbind(i,j)
  # class(toget_ij)
  print(toget_ij)
}

x <- matrix(1:6, 2, 3, byrow = T)
x


for(i in seq_len(nrow(x))) { # seq_len(nrow(x)): 1:nrow(x)
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }   
}

# Combination 
x <- runif(10, 3, 8)
x

mean(x)
# hist(x)
n <- length(x)

xnorm = xstandr = vector(mode="numeric", length= length(x))

for(i in 1:n){
  stopifnot(is.numeric(x))
  # Rescaling of the x values
  if(mean(x) > 5) {
    xnorm[i] <- (x[i]-min(x))/(max(x)-min(x))
  }
  else{
    xstandr[i] <- (x[i]-mean(x))/ sqrt(var(x))
  }
}

xnorm
hist(xnorm)
xstandr
hist(xstandr)

# While Loop --------------------------------------------------------------

# while (condition){
#   Do something
# }

i=2
while (i < 5){
  print(i)
  i <- i + 1
}


z <- 5

set.seed(11) # important for reproducibility

while (z >= 4 && z <= 10) { # & usage is possible or not ?
  coin <- rbinom(1, 1, 0.5)
  
  if (coin == 1) {
    ## random walk
    z <- z + 1
  } else {
    z <- z - 1
  }
}

z

# Break command -----------------------------------------------------------

no <- c(1:50)
for (val in no) 
{ 
  if (val %% 5 == 0)  
  { 
    print(paste("Coming out from for loop Where i = ", val)) 
    #next
  } 
  
  if (val %% 39 == 0)  
  { 
    print(paste("Coming out from for loop Where i = ", val)) 
    break
  } 
  print(paste("Values are: ", val)) 
} 

# Next command ------------------------------------------------------------

#next is used to skip an iteration of a loop.
x <- 1:50
for (val in x) {
  if (val %% 3 == 1){
    next
  }
  print(val)
  
}

# Functions ---------------------------------------------------------------

name <- function(arg1, arg2, ...)
  
# Importance of ...
  # Another frequent requirement is to allow one function to pass 
  # on argument settings to another

# function_name <- function(var){
#   Do something
#   return(new_variable)
# }

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}


square <- function(x){
  squared <- x*x
  return(squared)
}

# R functions arguments can be matched positionally or by name. 

str(rnorm)
mydata <- rnorm(100, 2, 1)              ## Generate some data
# hist(mydata)
rnor
x=matrix(rnorm(25), 5, 5, T)
x
is.matrix(x)

x <- as.data.frame(x)

# Taking the traspose of a matrix
mytrans <- function(x) {
  if (!is.matrix(x)) {
    #warning("argument is not a matrix: returning NA")
    #return(NA_real_)
    # OR 
    x <- as.matrix(x)
  }
  y <- matrix(1, nrow=ncol(x), ncol=nrow(x))
  
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      y[j,i] <- x[i,j]
      
      if(y[j,i] >= 0.5){
        y[j,i] <- 1
      }
      else{
        y[j,i] <- 0
      }
    }
  }
  return(list("x" = x, "y" = y)) # For multiple argument we need to use list general
}

str(mytrans)
mytrans(x)

# FOR SELF STUDY (BONUS)
# Summary function --------------------------------------------------------
rep <- 100
set.seed(1130)
df <- as.data.frame(cbind(rnorm(rep), runif(rep), 
                          sample(100,rep,replace = TRUE)))

SummaryLoc = function(data) {
  if (!is.data.frame(x)) {
    data <- as.data.frame(data)
  }
  
  n <- dim(data)[1]
  ncol <- dim(data)[2]
  
  dfsumm <- matrix(0,3,ncol)
  
  for (i in 1:ncol) {
    # For mean
    Sumdf <- lapply(data, sum)
    #class(Sumdf)
    dfsumm[1,i] <- unlist(Sumdf[i])/n
    
    # For median
    Sortdf=matrix(0,n,ncol)
    Sortdf[,i] <- sort(data[,i])
    #head(Sortdf)
    #class(Sortdf[,i])
    
    if(n %% 2 == 0) {
      dfsumm[2,i] <- sum(Sortdf[c(n/2,n/2+1),i]) / 2
    }
    
    if(n %% 2 == 1) {
      dfsumm[2,i] <- Sortdf[(n+1)/2,i]
    }
    
    # For Mode
    CountObs <- table(df[i])
    CountObs <- as.vector(CountObs)
    CountObs
    
    if (max(CountObs) > 1) {
      maxIndf <- which.max(table(df[i]))
      dfsumm[3,i] <- as.numeric(rownames(as.matrix(maxIndf)))
    }
    else {
      dfsumm[3,i] <- NaN
    }
    
  }
  # ,Output
  return(dfsumm)
}

# Performance Measuring ---------------------------------------------------

# KEEP in MIND THAT 
# Never grow a vector 

n=10000
system.time( x <- 1:n )

# OR Preallocate
x <- vector("numeric", n)
system.time(
  for(i in 1:n){
    x[i] <- i })

# OR  Growing
x <- NULL # Length zero
system.time(
  for(i in 1:n)
    x <- c(x, i)
)

# Vectorization
# use a vectorized solution wherever
# possible.

x <- vector("numeric", n)
system.time(
  for(i in 1:n)
    x[i] <- rnorm(1)
)

system.time(
  x <- rnorm(n)
)

# Another example 

x <- 1:100
## the vectorized version ...
system.time(
  y <- x^2)
y
## or the for loop version?

z <- vector(mode = mode(x), length = length(x))
system.time(
  for(i in seq_along(x)) {
    z[i] <- x[i]^2
  })

identical(y, z)

# For further information about timing/code profiling 
# Source: https://www.alexejgossmann.com/benchmarking_r/

# Other options 
# https://www.alexejgossmann.com/benchmarking_r/

# About profvis -----------------------------------------------------------
# Profvis is a tool for helping you to understand how R spends its time. 
# It provides a interactive graphical interface for visualizing data 
# from Rprof, R's built-in tool for collecting profiling data.

#' There are two ways to use profvis: 
#' From the Profile menu in RStudio. 
#' With profvis::profvis(). I recommend storing your code in 
#' a separate file and source()ing it in; this will 
#' ensure you get the best connection between profiling data and source code.

# About profiling the code, to understand which part is slow
install.packages("profvis")

library("profvis")

cities <- c("New York", "Paris", "London", "Leuven",
            "Ankara", "Dresden", "Padova")

num_chars <- c()

profvis({
  system.time(
    for(i in 1:length(cities)) {
      num_chars[i] <- nchar(cities[i]) })
  
})

#profvis({

# lapply function
system.time(
  lapply(cities, nchar)
)

#})

# To get unlist output
unlist(lapply(cities, nchar)) 

# CHECK other families of lapply; sapply, tapply etc. 
# https://www.guru99.com/r-apply-sapply-tapply.html


# Numeric Example
profvis({
  N <- 10000
  x1 <- rnorm(N)
  x2 <- rnorm(N)
  df <- as.data.frame(cbind(x1, x2))
  #df
  
  index <- length(df[, 1])
  # index <- dim(df)[1]
  
  #system.time( 
  for (loop in c(1:index)) {
    df$mean2[loop] <- mean(c(df[loop, 1], df[loop, 2]))
  }
  #)
  
  #system.time(
  df$mean1 <- apply(df, 1, mean)
  #  )
  
  #system.time(
  df$mean3 <- rowMeans(df[, c(1, 2)])
  #  )
})

# Another example for profvis

# Generate data
times <- 4e5
cols <- 150
data <- as.data.frame(x = matrix(rnorm(times * cols, mean = 5), ncol = cols))
data <- cbind(id = paste0("g", seq_len(times)), data)

profvis({
  data1 <- data
  # Four different ways of getting column means
  means <- apply(data1[, names(data1) != "id"], 2, mean)
  means <- colMeans(data1[, names(data1) != "id"])
  means <- lapply(data1[, names(data1) != "id"], mean)
  means <- vapply(data1[, names(data1) != "id"], mean, numeric(1))
})

# For further readings about profvis
# https://rstudio.github.io/profvis/index.html

# Microbenchmark -------------------------------------------------------------

# A microbenchmark is a measurement of the performance of a very small piece of code, 
# something that might take milliseconds (ms), microseconds (µs), 
# or nanoseconds (ns) to run. Microbenchmarks are useful for comparing small 
# snippets of code for specific tasks.

# A great tool is the bench package.
# The bench package uses a high precision timer, 
# making it possible to compare operations that only take a tiny amount of time
install.packages("bench")
library(bench)

x <- runif(100)

(lb <- bench::mark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1/2)
))


# bench::mark() returns the results as a tibble, 
# with one row for each input expression, and the followings:
# min, mean, median, max, and itr/sec summarise the time taken by the expression. 
# Focus on the minimum (the best possible running time) and the median (the typical time).
# mem_alloc tells you the amount of memory allocated by the first run
# n_gc() tells you the total number of garbage collections

lb[c("expression", "min", "median", "itr/sec", "n_gc")]

plot(lb)

# About the interpretation

# 1 ms, then one thousand calls take a second.
# 1 µs, then one million calls take a second.
# 1 ns, then one billion calls take a second.

# Example 

(lb <- bench::mark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1/2),
  exp(log(x) / 2)
))

(lb <- bench::mark(
  # Four different ways of getting column means
  apply(data[, names(data) != "id"], 2, mean),
  colMeans(data[, names(data) != "id"]),
  lapply(data[, names(data) != "id"], mean),
  vapply(data[, names(data) != "id"], mean, numeric(1))
, check = FALSE))

# Debugging Part ----------------------------------------------------------

# debugging is designed to help you find bugs by figuring out where 
# the code is not behaving in the way that you expect.

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}

# TRY 
f(5)

f("a")
traceback()
# From Debug button above ?

j <- function() k()
k <- function() 
  stop("Oops!", call. = TRUE) # Try FALSE 
f(j())
  
rlang::with_abort(f(j()))

rlang::last_trace()

# example of stopifnot

i <- function(d) {
    stopifnot(is.numeric(d) == TRUE)
  d + 10
}

f("a")

stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE
stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 > 2) 

# For interactive debugging

# you need more information, and the easiest way to get it is with the 
# interactive debugger which allows you to pause execution of a function 
# and interactively explore its state.

g <- function(b) {
  # browser() 
  h(b)
}
f(10)

# browser() is just a regular function call which means that you can run it conditionally
# by wrapping it in an if statement:

g <- function(b) {
  if (b < 0) {
    browser()
  }
  h(b)
}
f(10)

f(-5)

# About browser commands

# Some other alternatives
# recover function 

options(error = recover)
f("x")

options(error = NULL)

# Non interactive debugging part:

# When you can't explore interactively, 
# it's particularly important to spend some time making the problem as small 
# as possible so you can iterate quickly.

# dump.frames() is the equivalent to recover() for non-interactive code; 
# it saves a last.dump.rda file in the working directory.


# Pring debugging 
# where you insert numerous print statements to precisely locate the problem, 
# and see the values of important variables.

f <- function(a) {
  cat("f()\n")
  g(a)
}
g <- function(b) {
  cat("g()\n")
  cat("b =", b, "\n")
  h(b)
}
h <- function(c) {
  cat("i()\n")
  i(c)
}

f(10)
f("a")

# For Non-error failures
# There are other ways for a function to fail apart from throwing an error:

f <- function() g()
g <- function() warning("Hi!")
f()
options(warn = 2) # Convert into error
f()
options(warn = 1) # to keep it as warning

f <- function() g()
g <- function() message("Hi!")
f()


rlang::with_abort(f(), "message")
rlang::last_trace()

# Other sources
# https://data-flair.training/blogs/debugging-in-r-programming/

# For automatization 

# You can automate this process with the errorist and searcher packages
# https://cran.r-project.org/web/packages/errorist/index.html
# https://cran.r-project.org/web/packages/searcher/searcher.pdf


# try ----------------------------------------------------------------

try(f(10))

try(f("a"))

tryCatch(f("a"))

# Example 

nums = list(12,88,39,"Ten",51,12)

div_by_3 = function(n){
  return(n/3)
}

#works fine
div_by_3(nums[[1]])
div_by_3(nums[[4]])

divided_output = sapply(nums, function(x){ div_by_3(x) })

# Using tryCatch, we can isolate the error to only those members of the list 
# that caused the problem.

divided_out = sapply(nums, function(x){
  tryCatch(
    #this is the chunk of code we want to run
    {div_by_3(x)
      #when it throws an error, the following block catches the error and return NA
    }, error = function(msg){
      return(NA)
    })
})






