library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(discrim)
library(themis)
library(naivebayes)
library(recipes)
library(hardhat)
library(glmnet)
library(utiml)
library(tm)
library(caret)
library(kableExtra)
library(kknn)


# Load the cleaned data
articles_text_clean <- read_csv(here::here("data/processed/clean_text_2024-09-16.csv"))

# drop any rows with NA for Value_Orientation
articles_text_clean <- articles_text_clean[!is.na(articles_text_clean$Value_Orientation), ]

# Load the unlabeled documents
unlabeled_articles <- read_csv(here::here("data/processed/newdata_for_model_testing_text_2024-11-06.csv"))
unlabeled_articles <- unlabeled_articles %>%
  dplyr::select(Article_Text)


text2wvo <- articles_text_clean %>%
  dplyr::select(Article_Text, Value_Orientation)

text2wvo$Value_Orientation <- as.factor(text2wvo$Value_Orientation)

text_split <- initial_split(text2wvo, strat = Value_Orientation)

text_train <- training(text_split)
text_test <- testing(text_split)


text_rec <- 
  recipe(Value_Orientation ~ Article_Text, data = text_train)

text_rec <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_tokenfilter(Article_Text, max_tokens = 1000) %>%
  step_tfidf(Article_Text)
#step_downsample(Value_Orientation) #downscale for unbalanced data

set.seed(2345)
text_folds <- vfold_cv(text_train)

text_wf <- workflow() %>%
  add_recipe(text_rec)

# parameter tuning 
cntrl <- control_resamples(save_pred = TRUE)
svm_tune_grid <- grid_regular(
  cost(range = c(-10,5))
)

svm_spec <- svm_rbf(cost = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_spec

