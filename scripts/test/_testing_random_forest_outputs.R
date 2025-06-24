library(ranger)
library(tidyverse)

## Classification forest
ranger(Species ~ ., data = iris)
train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris.train <- iris[train.idx, ]
iris.test <- iris[-train.idx, ]
rg.iris <- ranger(Species ~ ., data = iris.train, probability = TRUE) # set probability to true
pred.iris <- predict(rg.iris, data = iris.test)
table(iris.test$Species, pred.iris$predictions)

pred.iris$predictions # with probability = TRUE, the predictions are now the probabilities of a class instead of a single class

