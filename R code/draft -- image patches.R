# read the data into r
train <- read.csv("training.csv", header = T, stringsAsFactors = F)
test <- read.csv("test.csv", header = T, stringsAsFactors = F)

# convert the string representation of image into integers
im1 <- train$Image
im2 <- test$Image

# specify parameters
patch_size <- 10
search_size <- 2

# parallel computation
library(doSNOW)
library(foreach)

# implement the parallelization
NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

# convert string into integer in image column
img.train <- as.integer(unlist(strsplit(im1, " ")))
img.test <- as.integer(unlist(strsplit(im2, " ")))

stopCluster(cl) # close clusters

# update our train and test data without the image column
train$Image <- NULL
test$Image  <- NULL

# list the names of the 15 keypoints we have to predict
coordinate.names <- gsub("_x", "", names(train)[grep("_x", names(train))])


# for each facial keypoint, compute the average patch

# implement the parallelization
NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

mean.patches <- foreach(coord = coordinate.names) %dopar% {
  cat(sprintf("computing mean patch for %s\n", coord))
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")
}
stopCluster(cl) # close clusters



# compute average patch

# implement the parallelization
NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

patches <- foreach(i = 1:nrow(train), .combine="rbind") %do% {
  im1  <- matrix(data = img.train[i,], nrow=96, ncol=96)
  x   <- train[i, coord_x]
  y   <- train[i, coord_y]
  x1  <- (x-patch_size)
  x2  <- (x+patch_size)
  y1  <- (y-patch_size)
  y2  <- (y+patch_size)
  if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
  {as.vector(im1[x1:x2, y1:y2])}
  else {NULL}
}
stopCluster(cl) # close clusters

matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)
  
