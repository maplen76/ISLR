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

