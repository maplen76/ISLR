library(ISLR)
names(Smarket)

# logistics Regression
summary(Smarket)
pairs(Smarket) # to create the scatter plot matrix

cor(Smarket[,-9]) # produces correlation matrix

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = Smarket, family = binomial) # family = binomial is meant to run logistics regression

summary(glm.fits)
coef(glm.fits)

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]

# subset data
attach(Smarket)
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,]
Direction.2005 <- Direction[!train]

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) # compute fraction of prediction was correct

# LDA, lda() function is part of MASS library
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
plot(lda.fit) # produces plots of linear discriminants
lda.pred <- predict(lda.fit, Smarket.2005) # return a list with three elements
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

# QDA qda() function is part of MASS library
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

# K-Nearest Neighbors knn() is part of the class library
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
text.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
# try to increase K further to improve the prediction, but it turns out to provide no further improvements
knn.pred <- knn(train.X, text.X, train.Direction, k = 3) 
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

## An Application to Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)

# scale() function standardize the data so that all variables are given a mean of zero and standard deviation of 1
# the reason why standardize is to exlude the influence by very large scale
standardized.X <- scale(Caravan[, -86])

# split the observations into a test set
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train = train.X, test = test.X, cl = train.Y, k = 1)
mean(test.Y !=  knn.pred)
