library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(discrim)
library(naivebayes)
library(recipes)
library(hardhat)
library(glmnet)
library(utiml)
library(tm)
library(caret)


# Load the cleaned data
articles_text_clean <- read_csv(here::here("data/processed/clean_text_2024-04-09.csv"))

## Add a document id column
articles_text_clean$id <- seq.int(nrow(articles_text_clean))

## try out some more data transformation, for Focus, Conflict_type, and
## Value_Orientation I want these columns to hold the list of codes for 
## each article. nest() is what I want.

test_df <- articles_text_clean %>%
  select(Title.x, Focus, Conflict_Type, Article_Text, Value_Orientation) %>%
  group_by(Title.x) %>%
  nest("multi_labels" = c(Focus, Conflict_Type, Value_Orientation)) %>%
  select(Title.x, multi_labels, Article_Text)
  


## A few articles have a bunch of html code at the end so remove that
### may want to add this as an if-else to clean up the larger corpus
for (i in 1:length(articles_text_clean)) {
  if (str_detect(articles_text_clean[[6]][i], "</div>") == TRUE) {
    articles_text_clean[[6]][i] <- str_extract(articles_text_clean[[6]][15], regex(".+?(?=</div>)"))
  } else {articles_text_clean[[6]][i] <- articles_text_clean[[6]][i]
  }
}

# tokenize and create dtm first?
tidy_text <-  articles_text_clean %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word))  

## Remove stop words
## Create a small data frame of your own stop words for this project 
data("stop_words")
wildlife_stop_words <- data.frame(c("p", "br", "strong", "targetednews.com",
                                 "grizzly", "grizzlies", "bears", "bear", 
                                 "wolf", "wolves", "coyote", "coyotes", 
                                 "pigs", "pig", "beaver", "beavers")) 
colnames(wildlife_stop_words) <-("word")

tidy_text_stop <- tidy_text %>%
  anti_join(stop_words) %>%
  anti_join(wildlife_stop_words)

dtm <- tidy_text_stop %>%
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)

# Split the data: create training, testing, and labels datasets
articles_text_clean$Focus <- as.factor(articles_text_clean$Focus)
articles_text_clean$Conflict_Type <- as.factor(articles_text_clean$Conflict_Type)
articles_text_clean$Value_Orientation <- as.factor(articles_text_clean$Value_Orientation)
articles_text_clean <- articles_text_clean %>%
  filter(is.na(Focus) == FALSE)

trainIndex <- createDataPartition(y = articles_text_clean$id, p = 0.7,list = FALSE)
testIndex <- articles_text_clean$id[-trainIndex]

set.seed(455)
data_to_train <- dtm[trainIndex, ] %>% as.matrix() %>% as.data.frame() 
data_to_test <- dtm[testIndex, ] %>% as.matrix() %>% as.data.frame()
label_train <- articles_text_clean$Value_Orientation[trainIndex]
label_test <- articles_text_clean$Value_Orientation[testIndex]

## Classification...here we go!

# KNN
# 1. Train the model
knn_model <- train(x = data_to_train, #training data
                   y = as.factor(label_train), #labeled data
                   method = "knn", #the algorithm
                   trControl = fitControl, #the resampling strategy we will use
                   #tuneGrid = data.frame(k = 2) #the hyperparameter
)

# 2. Test the trained model on the test data
knn_predict <- predict(knn_model, newdata = data_to_test)
# 3. Check the model performance
# You can look at a confusion matrix to see how well the model did
knn_confusion_matrix <- confusionMatrix(knn_predict, label_test, mode = "prec_recall")
knn_confusion_matrix

# SVM
# 1. Train the model on the training data
svm_model <- caret::train(x = data_to_train,
                          y = as.factor(label_train),
                          method = "svmLinear3",
                          #trControl = trctrl, 
                          tuneGrid = data.frame(cost = 1, #accounts for over-fitting
                                                Loss = 2)) #accounts for misclassifications
# 2. Test the trained model on the test data
svm_predict <- predict(svm_model, newdata = data_to_test)
# 3. Check the model performance
# You can look at a confusion matrix to see how well the model did
svm_confusion_matrix <- confusionMatrix(svm_predict, label_test, mode = "prec_recall")
svm_confusion_matrix

# Decision trees
# 1. Train the model on the training data
dt_mod <- train(x = data_to_train,
                y = as.factor(label_train),
                method = "rpart",
                #trControl = trctrl
)
# 2. Test the trained model on the test data
dt_predict <- predict(dt_mod, newdata = data_to_test) 
# 3. Check the model performance
# You can look at a confusion matrix to see how well the model did
dt_confusion_matrix <- confusionMatrix(dt_predict, label_test, mode = "prec_recall")
dt_confusion_matrix

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
# Random Forests
# 1. Train the model on the training data
rf_mod <- train(x = data_to_train,
                y = as.factor(label_train),
                method = "ranger",
                trControl = fitControl,
                #tuneGrid = data.frame(mtry = floor(sqrt(dim(data_to_train)[2])),
                #                      splitrule = "extratrees",
                #                      min.node.size = 1)
                )

rf_mod

# 2. Test the trained model on the test data
rf_predict <- predict(rf_mod, newdata = data_to_test)
# 3. Check the model performance
# You can look at a confusion matrix to see how well the model did
rf_confusion_matrix <- confusionMatrix(rf_predict, label_test, mode = "prec_recall")
rf_confusion_matrix

# Naive Bayes
library(e1071)

# 1. Train the model on the training data
nb_mod = train(x = data_to_train,
               y = as.factor(label_train),
               mthos = 'nb',
               trControl=trainControl(method='cv',number=10)
               )
# 2. Test the trained model on the test data
nb_predict <- predict(nb_mod, newdata = data_to_test)
# 3. Check the model performance
# You can look at a confusion matrix to see how well the model did
nb_confusion_matrix <- confusionMatrix(nb_predict, label_test, mode = "prec_recall")
nb_confusion_matrix


