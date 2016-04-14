# search for the keypoint in the test images

# define a new parameter, which indicates how many pixels we are going to move in each 
# direction when searching for the keypoint
search_size <- 2

# center the search on the average keypoint location
mean_x <- mean(sm.train1[, coord_x], na.rm=T)
mean_y <- mean(sm.train1[, coord_y], na.rm=T)

# go search_size pixels in each direction
x1 <- as.integer(mean_x)-search_size
x2 <- as.integer(mean_x)+search_size
y1 <- as.integer(mean_y)-search_size
y2 <- as.integer(mean_y)+search_size

# build a dataframe with all combinations of x's and y's
params <- expand.grid(x = x1:x2, y = y1:y2)
params

# try all these combinations on a single test image
im <- matrix(data = img.test[1,], nrow=96, ncol=96)

# use doSNOW to conduct parallel computation
library(doSNOW)
NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

r <- foreach(j = 1:nrow(params), .combine=rbind) %dopar% {
  x <- params$x[j]
  y <- params$y[j]
  p <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
  score <- cor(as.vector(p), as.vector(mean.patch))
  score <- ifelse(is.na(score), 0, score)
  data.frame(x, y, score)
}
stopCluster(cl) # close clusters

# return the coordinate with the highest score
best <- r[which.max(r$score), c("x", "y")]
best
