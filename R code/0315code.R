## 0315
# goal: locate specific keypoints on face images

# read the data into r
train <- read.csv("training.csv", stringsAsFactors = F)
str(train)
dim(train)

# get a small subset of the data
sm.train <- train[1:50,]
str(sm.train)
dim(sm.train)

# write a csv file of the small data
write.csv(sm.train, file = "sm_train.csv")

## 0316
# choose only the first 30 columns in the dataset
sm.train1 <- sm.train[,1:30]

# convert the string representation of image into integers
as.integer(unlist(strsplit(sm.train$Image[1], " ")))

# packages that allow parallel computation to same time 
# when apply function to the whole dataset
library(foreach)
library(doSNOW)

# implement the parallelization, convert string into integer in image column
NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

# the parallel programming code
img.train <- foreach(im = sm.train$Image, .combine = "rbind") %dopar% {
  as.integer(unlist(strsplit(im, " ")))}

stopCluster(cl) # close clusters

# now it has 9216 columns, one for each pixel
str(img.train)

# repeat the process for test data

# first read the test data into r
test <- read.csv("test.csv", header = T, stringsAsFactors = F)

NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

# the parallel programming code
img.test <- foreach(im = test$Image, .combine = "rbind") %dopar% 
  {as.integer(unlist(strsplit(im, " ")))}

stopCluster(cl) # close clusters

# now it has 9216 columns, one for each pixel
str(img.test)

# save all the data as an R data file 
save(train, test, img.train, img.test, im1, im2,
     file = "kaggledata.rd")

# reload them at any time with the following code
load("kaggledata.rd")


## 0317
# looking at the data

# convert these image integers into a 96*96 matrix
# 'rev' reverse the resulting vector to match the interpretation of R's 
# image function (which expects the origin to be in the lower left corner)
im <- matrix(data=rev(img.train[1,]), nrow=96, ncol=96)
dim(im)

# visualize the image
image(1:96, 1:96, im, col=gray((0:255)/255))

# add some keypoints from the training data to check if everything is correct
# color the coordinates for the eyes and nose
# adjust the coordinates for the different origins
points(96-sm.train$nose_tip_x[1], 96-sm.train$nose_tip_y[1], col="red")
points(96-sm.train$left_eye_center_x[1], 96-sm.train$left_eye_center_y[1], col="blue")
points(96-sm.train$right_eye_center_x[1], 96-sm.train$right_eye_center_y[1], col="green")

# where are the centers of each nose in this dataset
for(i in 1:nrow(sm.train)) {
  points(96-sm.train$nose_tip_x[i], 
         96-sm.train$nose_tip_y[i], col="red")}

# starting point: compute the mean for each keypoint in the training dataset
# and use that as a prediction for all images
colMeans(sm.train1, na.rm = T)

# apply these computed coordinates to the test data
dim(sm.train1)
dim(img.test)

p <- matrix(data=colMeans(sm.train1, na.rm=T), 
            nrow=nrow(img.test), 
            ncol=ncol(sm.train1), byrow=T)

colnames(p) <- names(sm.train1)

dim(p)
head(p)

predictions <- data.frame(ImageId = 1:nrow(img.test), p)
head(predictions)

# submission format: one keypoint per row
library(reshape2)
submission <- melt(predictions, id.vars="ImageId", 
                   variable.name="FeatureName", value.name="Location")
head(submission)
dim(submission)

# write a csv file to preserve the submission file format
write.csv(submission, file = "SubmissionFileFormat.csv")

# produce submission file
idlook <- read.csv("IdLookupTable.csv")
idlook$Location <- NULL
example.submission <- merge(idlook, submission, all.x = T)
dim(example.submission)
write.csv(example.submission, file = "submission_means.csv")
