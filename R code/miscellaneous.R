# read the data into r
train <- read.csv("sm_train.csv", header = T, stringsAsFactors = F)


# packages that allow parallel computation to same time 
# when apply function to the whole dataset
library(foreach)
library(doSNOW)

# implement the parallelization, convert string into integer in image column
NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

# the parallel programming code
img.train <- foreach(im = train$Image, .combine = "rbind") %dopar% {
  as.integer(unlist(strsplit(im, " ")))}

stopCluster(cl) # close clusters

# now it has 9216 columns, one for each pixel
str(img.train)

# repeat the process for test data

# first read the test data into r
test <- read.csv("test.csv", header = T, stringsAsFactors = F)
test1 = test[1:50,]

NumberOfCluster <- 4 # number of jobs the laptop do at the same time
cl <- makeCluster(NumberOfCluster) # make clusters
registerDoSNOW(cl) # use the above cluster

# the parallel programming code
img.test <- foreach(im = test1$Image, .combine = "rbind") %dopar% 
{as.integer(unlist(strsplit(im, " ")))}

stopCluster(cl) # close clusters

# now it has 9216 columns, one for each pixel
str(img.test)


# convert these image integers into a 96*96 matrix
# 'rev' reverse the resulting vector to match the interpretation of R's 
# image function (which expects the origin to be in the lower left corner)
im <- matrix(data=rev(img.train[5,]), nrow=96, ncol=96)
dim(im)

# visualize the image
image(1:96, 1:96, im, col=gray((0:255)/255))

# add some keypoints from the training data to check if everything is correct
# color the coordinates for the eyes and nose
# adjust the coordinates for the different origins
points(96-train$nose_tip_x[1], 96-train$nose_tip_y[1], col="red")
points(96-train$left_eye_center_x[1], 96-train$left_eye_center_y[1], col="blue")
points(96-train$right_eye_center_x[1], 96-train$right_eye_center_y[1], col="green")

# where are the centers of each left eye in this dataset
for(i in 1:nrow(train)) {
  points(96-train$left_eye_center_x[i], 
         96-train$left_eye_center_y[i], col="red")}


library(e1071)
eyex = train$left_eye_center_x
eye.df = data.frame(eyex, img.train)
attach(eye.df)

svmfit =svm(eyex~., data=train, kernel="radial", gamma=1, cost=1)
summary(svmfit)
plot(svmfit, train)

tune.out=tune(svm, eyex~., data=train, kernel="radial",
              ranges=list(cost=c(0.1 ,1 ,10 ,100 ,1000), 
                          gamma=c(0.5,1,2,3,4) ))
summary(tune.out)

# perform best subset selection
library(leaps)
regfit.full = regsubsets(eyex~., data = eye.df, really.big=T)
summary(regfit.full)

## 1. histogram stretching
for (i in 1:nrow(img.train)){
  a = min(img.train[i,])
  b = max(img.train[i,])
  for (n in 1:ncol(img.train)){
    newdat = img.train
    newdat[i,n] = (img.train[i,n]-a)/(b-a)*255}
}


## 2. PCA


