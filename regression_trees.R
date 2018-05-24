#estimating the quality of wines with regression trees and model trees
wine <- read.csv("whitewines.csv")
str(wine)
hist(wine$quality)
summary(wine)
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]
install.packages("rpart")
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)
#mean of the absolute value of the errors
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.rpart, wine_test$quality)
mean(wine_train$quality)
MAE(5.87, wine_test$quality)
#Recall that a model tree improves on regression trees by replacing the leaf nodes with regression
#models. This often results in more accurate results than regression trees, which use
#only a single value for prediction at the leaf nodes.
#The current state-of-the-art in model trees is the M5' algorithm (M5-prime) by
#Wang and Witten, which is an enhancement of the original M5 model tree algorithm
#proposed by Quinlan in 1992.
install.packages("RWeka")
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)
