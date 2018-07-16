# tree based method

library(tree)
library(ISLR)
library(dplyr)
library(ggplot2)
library(tidyr)

Carseats <- Carseats %>%
    mutate(High = ifelse(Sales <= 8, "No", "Yes"),
           High = as.factor(High))

# the response have to be factor when implement classification tree
tree.carseats <- tree(High ~ .-Sales, data = Carseats)

summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]

tree.carseats.train <- tree(High ~ .-Sales, data = Carseats, subset = train)
tree.pred <- predict(object = tree.carseats.train, newdata = Carseats.test, type = "class")
table(tree.pred, Carseats.test$High)

set.seed(3)
cv.carseats <- cv.tree(tree.carseats.train, FUN = prune.misclass)

cv.carseats[1:3] %>%
    as.data.frame() %>%
    gather(m,value,size,k) %>%
    ggplot(aes(x = value, y = dev)) +
    geom_point() +
    geom_line() +
    facet_wrap(~m)

# as the graph tell optimal node is 9
prune.carseats <- prune.misclass(tree = tree.carseats.train, best = 9)
tree.pred.prune <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred.prune, Carseats.test$High)

plot(prune.carseats)
text(prune.carseats, pretty = 0
