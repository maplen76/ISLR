# Linear Regression
library(MASS)
library(ISLR)
data(Boston) # load Boston data
fix(Boston)

# Simple Linear Regression

# medv as the respnse and lastat as the predictor
lm.fit <- lm(medv ~ lstat, data = Boston) # model syntax, y ~ x

summary(lm.fit) # summary of the linear regression modle
coef(lm.fit) # coef to get the coefficients
confint(lm.fit) # confint to get the confidence interval
predict(object = lm.fit, newdata = data.frame(lstat = c(5,10,15)), interval = "confidence") # predict function to produce confidence intervals and prediction intervals for the prediction

plot(x = Boston$lstat, y = Boston$medv)
abline(lm.fit)

# use ggplot method to generate the plot with confidence interval
ggplot(data = Boston, mapping = aes(x = lstat, y = medv)) + 
    geom_point() +
    scale_y_continuous(limits = c(0,NA)) +
    geom_smooth(method = "lm", se = T)


# disgnostic plots
par(mfrow = c(2,2)) # par is set to view all 4 plots together
plot(lm.fit) # call plot with the model to generate diagnostics plots

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))

# Multiple Linear Regression
lm.fit <- lm(medv ~ lstat + age, data = Boston)
lm.fit <- lm(medv ~ ., data = Boston) # perform regression using all of variables
summary(lm.fit)
lm.fit1 <- lm(medv ~ .-age, data = Boston) # perform regression using all of variables but one

# interaction terms
summary(lm(medv ~ lstat * age, data = Boston)) # lstat * age inludes lstat, age, lstat*age as predictors

lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston) # function I() is needed to perform Non-liner transforamtion
lm.fit <- lm(medv ~ lstat, data = Boston)
summary(lm.fit2)
anova(lm.fit, lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5), data = Boston) # use polynomial model
summary(lm.fit5)


# for Qualitative Predictors, R will generate dummy variables
data(Carseats)
