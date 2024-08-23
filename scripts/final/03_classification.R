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
articles_text_clean <- read_csv(here::here("data/processed/clean_text_2024-07-25.csv"))

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


# Create simplified value_orientation
articles_text_clean$value_simple <- as.factor(articles_text_clean$Value_Orientation)
articles_text_clean=within(articles_text_clean,{
  value_simple=NA
  value_simple[Value_Orientation == "1" | Value_Orientation == "2"] = "Mutualistic"
  value_simple[Value_Orientation == "3" | Value_Orientation == "4" | Value_Orientation == "5"] = "Neutral"
  value_simple[Value_Orientation == "6" | Value_Orientation == "7"] = "Domination"
})

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
                                 "pigs", "pig", "beaver", "beavers", 
                                 "amp", "div", "class", "span", "href",
                                 "wildlife", "wild", "fish", "animal",
                                 "animals", "species")) 
colnames(wildlife_stop_words) <-("word")

tidy_text_stop <- articles_text_clean %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word)) %>%
  anti_join(stop_words) %>%
  anti_join(wildlife_stop_words)

dtm_stop <- tidy_text_stop %>%
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)

dtm <- tidy_text %>%
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)


# Split the data: create training, testing, and labels datasets
articles_text_clean$Focus <- as.factor(articles_text_clean$Focus)
articles_text_clean$Conflict_Type <- as.factor(articles_text_clean$Conflict_Type)
articles_text_clean$Value_Orientation <- as.factor(articles_text_clean$Value_Orientation)
#articles_text_clean$value_simple <- as.factor(articles_text_clean$value_simple)
articles_text_clean <- articles_text_clean %>%
  filter(is.na(Focus) == FALSE)

trainIndex <- createDataPartition(y = articles_text_clean$id, p = 0.7,list = FALSE)
testIndex <- articles_text_clean$id[-trainIndex]

set.seed(455)
data_to_train <- dtm_stop[trainIndex, ] %>% as.matrix() %>% as.data.frame() 
data_to_test <- dtm_stop[testIndex, ] %>% as.matrix() %>% as.data.frame()
label_train <- articles_text_clean$Value_Orientation[trainIndex]
label_test <- articles_text_clean$Value_Orientation[testIndex]

## Quick look at common words in value_simple
tidy_text_stop %>%
  group_by(Value_Orientation) %>%
  count(word) %>%
  filter(n > 200) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) + 
  facet_wrap(~Value_Orientation)


## Classification...here we go!
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  #summaryFunction = ,
    ## repeated ten times
    repeats = 10)

# KNN
# 1. Train the model
knn_model <- train(x = data_to_train, #training data
                   y = as.factor(label_train), #labeled data
                   method = "knn", #the algorithm
                   trControl = fitControl, #the resampling strategy we will use
                   metric = "Kappa",
                   #tuneGrid = data.frame(k = 2) #the hyperparameter or param-tuning
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
saveRDS(svm_model, here::here(paste0("output/svm_mod_", Sys.Date(), ".rds")))

# 2. Test the trained model on the test data
svm_predict <- predict(svm_model, newdata = data_to_test)
# 3. Check the model performance
# You can look at a confusion matrix to see how well the model did
svm_confusion_matrix <- confusionMatrix(svm_predict, label_test, mode = "prec_recall")
svm_confusion_matrix
saveRDS(svm_confusion_matrix, here::here(paste0("output/svm_mod_confusion_", Sys.Date(), ".rds")))

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
saveRDS(rf_mod, here::here(paste0("output/rf_mod_", Sys.Date(), ".rds")))

# 2. Test the trained model on the test data
rf_predict <- predict(rf_mod, newdata = data_to_test)
# 3. Check the model performance
# You can look at a confusion matrix to see how well the model did
rf_confusion_matrix <- confusionMatrix(rf_predict, label_test, mode = "prec_recall")
rf_confusion_matrix
saveRDS(rf_confusion_matrix, here::here(paste0("output/rf_mod_confusion_", Sys.Date(), ".rds")))

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

#---Would these models work better with bi- or tri-grams?----

tidy_bigram <-  articles_text_clean %>%
  unnest_tokens(bigram, Article_Text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  filter(!grepl('[0-9]', bigram))  

bigrams_separated <- tidy_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% wildlife_stop_words$word) %>%
  filter(!word2 %in% wildlife_stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

dtm_bigram <- bigrams_united %>%
  count(id, bigram, sort = TRUE) %>%
  bind_tf_idf(bigram, id, n) %>%
  cast_dtm(id, bigram, tf_idf)

set.seed(455)
bigram_to_train <- dtm_bigram[trainIndex, ] %>% as.matrix() %>% as.data.frame() 
bigram_to_test <- dtm_bigram[testIndex, ] %>% as.matrix() %>% as.data.frame()
label_train <- articles_text_clean$Focus[trainIndex]
label_test <- articles_text_clean$Focus[testIndex]
