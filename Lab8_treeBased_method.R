# Chapter 8 Lab: Decision Trees ################################################

## Fitting Classification Trees

library(tree)
library(ISLR)
library(dplyr)

Carseats <- Carseats %>%
    mutate(High = ifelse(Sales<=8,"No","Yes"),
           High = as.factor(High))   # left side must be numerical or factor

## 1. fit classification with all variables but sales (which is the response)
tree.carseats <- tree(High ~ .-Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

## 2. split observations into training set and test set
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
tree.carseats <- tree(High~.-Sales,Carseats,subset=train)
tree.pred <- predict(tree.carseats,Carseats.test,type="class")

table(tree.pred,Carseats.test$High)
(86+57)/200

## 3. consider if pruning the tree might lead to improved results
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

## 4. step3 found tree with 9 terminal nodes results in the loweset cross-validation error rate
prune.carseats <- prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)

## 4.1 used the pruned tree to validate the test set
tree.pred <- predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,Carseats.test$High)
(94+60)/200

## 4.2 to incresae the value of best, obtain larger pruned tree with lower classification accuracy
prune.carseats <- prune.misclass(tree.carseats,best=12)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred, Carseats.test$High)
(86+62)/200

## Fitting Regression Trees
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~.,Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

## use cv.tree funtion to see if pruning will improve performance
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

prune.boston <- prune.tree(tree.boston,best = 7)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat <- predict(tree.boston, newdata=Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Bagging and Random Forests
library(randomForest)
set.seed(1)
# when mtry equals number of predictor, it implement bagging method
bag.boston <- randomForest(medv~., data = Boston, subset = train, mtry = 13,importance = TRUE)
bag.boston

## to see how well dose bagged model performed on the test set
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

## to perform bagging method by change number of trees
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean( (yhat.bag - boston.test)^2 )
set.seed(1)

## perform random forest
rf.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf <- predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

## Boosting
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost <- predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

boost.boston <- gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost <- predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
