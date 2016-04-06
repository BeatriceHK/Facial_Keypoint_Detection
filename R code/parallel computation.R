library(parallel)

# number of cores in this laptop
# This number devided by 2 is the number of cores in this laptop
detectCores()

# foreach
library(doSNOW)
# we get result as list
foreach(i = 1:3) %do% {sqrt(i)}

# we get result as vector with using .combine="c" option
foreach(i = 1:3,.combine = "c") %do% {sqrt(i)}

# if a result is "vector",we can get it as matrix with using 
# .combine="cbind" option
foreach(i = 1:3,.combine = "cbind") %do% {letters[1:4]}

# if you define a function,you can use it as .combine option
# I wrote my function as returning same result that specify .combine="c" 
MyFunc <- function(x,y)c(x,y)
foreach(i = 1:3, .combine = "MyFunc") %do% {sqrt(i)}


# doSNOW
library(doSNOW)

getDoParWorkers()
getDoParName()
getDoParVersion()

registerDoSNOW(makeCluster(2, type = "SOCK"))

getDoParWorkers()
getDoParName()
getDoParVersion()

# compare the following processing time
N <- 10^4

system.time(foreach(i = 1:N,.combine = "cbind") %do% {sum(rnorm(N))})

system.time(foreach(i = 1:N,.combine = "cbind") %dopar% {sum(rnorm(N))})


# how many jobs you want the computer to run at the same time
NumberOfCluster <- 4

#------------------------------------
cl <- makeCluster(NumberOfCluster) # Make clusters
registerDoSNOW(cl) # use the above cluster
# your parallel programming code code code
stopCluster(cl) # close clusters
#------------------------------------


# compare several ways

# 1. for loop
x <- matrix(NA, ncol = 5, nrow = 5)
for (i in 1:5) {x[i, ] <- (1:5)^i}
x

# 2. foreach
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

x0 <- foreach(i = 1:5) %dopar% {
  y <- (1:5)^i 
  return(y)
  }
class(x0)

# 3. foreach, use .combine
x <- foreach(i = 1:5, .combine = "rbind") %dopar% {
  y <- (1:5)^i
  return(y)
  }
stopCluster(cl)
x
