# fitting a neural network
set.seed(500)
library(MASS)
data <- Boston

# check that no data point is missing
apply(data, 2, function(x) sum(is.na(x)))

# randomly splitting the data into a train and a test set
# then fit a linear regression, and test it on the test set
index <- sample(1:nrow(data), round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

lm.fit <- glm(medv~., data=train)
summary(lm.fit)

pr.lm <- predict(lm.fit, test)

# use the MSE as a measure of how much our predictions are far away
# from the real data
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

# normalize your data before training a neural network
# use min-max scale method to scale the data in the interval [0,1]
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

# scale returns a matrix that needs to be coerced into a data.frame.
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

# fit the neural net
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

# linear.output: specify whether we want to do regression or classification
nn <- neuralnet(f, data=train_, hidden=c(5,3), linear.output=T)

# graphical representation of the model 
# with the weights on each connection
# blue lines: the bias term added in each step
plot(nn)


# predict values for the test set and calculate the MSE

# predict values for the test set
pr.nn <- compute(nn, test_[,1:13])
summary(pr.nn)

# scale the result back
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)


# compare the two MSEs
print(paste(MSE.lm,MSE.nn))


# visual approach to the performance of the network
par(mfrow=c(1,2))

plot(test$medv, pr.nn_, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright', legend='NN', pch=18, col='red', bty='n')

plot(test$medv, pr.lm, col='blue', main='Real vs predicted lm', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright', legend='LM', pch=18, col='blue', bty='n', cex=.95)

par(mfrow=c(1,1))


# in one single plot
plot(test$medv, pr.nn_, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
points(test$medv, pr.lm, col='blue', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright', legend=c('NN','LM'), pch=18, col=c('red','blue'))


# implement a 10-fold cross validation
library(boot)
set.seed(200)
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]


# split the data in this way: 90% train set and 10% test set 
# in a random way for 10 times
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

# calculate the average MSE
mean(cv.error)
cv.error

# plot the results as a boxplot
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)





