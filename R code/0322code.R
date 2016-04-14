# first image in the training data set
im <- matrix(data = im.train[1], nrow=96, ncol=96)

# the right_eyebrow_inner_end coordinate of the first image in the training data
x <- sm.train[17, 1]
y <- sm.train[17, 2]

# the coordinates of the patch
x1 <- (x-patch_size)
x2  <- (x+patch_size)
y1  <- (y-patch_size)
y2  <- (y+patch_size)

# return the image patch as a vector
as.vector(im[x1:x2,y1:y2])
