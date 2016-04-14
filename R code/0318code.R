# using image patches
coord <- "mouth_center_bottom_lip"
patch_size <- 10 # the number of pixels we extract in each direction

# we will have a square of 21*21 pixels 
coord_x <- paste(coord, "x", sep="_") # left_eye_center_x
coord_y <- paste(coord, "y", sep="_") # left_eye_center_y


# use doSNOW to conduct parallel computation
library(doSNOW)

NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

# the parallel programming code
patches <- foreach(i = 1:nrow(sm.train), .combine = "rbind") %dopar% {
  im <- matrix(data = im.train[i,], nrow=96, ncol=96)
  x <- sm.train[i, coord_x]
  y <- sm.train[i, coord_y]
  x1 <- (x-patch_size)
  x2 <- (x+patch_size)
  y1 <- (y-patch_size)
  y2 <- (y+patch_size)
  if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96))
  {as.vector(im[x1:x2, y1:y2])}
  else
  {NULL}
}

stopCluster(cl) # close clusters

dim(patches)
head(patches)

mean.patch <- matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)

image(1:21, 1:21, mean.patch[21:1,21:1], col=gray((0:255)/255))

