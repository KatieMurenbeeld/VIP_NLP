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
library(stopwords)

# basic steps:
# 1. split the data into training and testing sets
# 2. create a text recipe (model, value orientation from text) on the training set
# 3. add preprocessing steps to the text recipe
# 4. create a tuneable model (multinomial, knn, svm, rf, etc)
# 5. create a model workflow by adding the text recipe and the model
# 6. tune the model using the cross validation folds created from the training data and a tune grid with the ranges for the (hyper)parameters
# 7. use last_fit() to "fit our model one last time on the training data  and evaluate it on our testing data"
# 8. extract and save the final fitted model

# Load the cleaned data
articles_text_clean <- read_csv(here::here("data/processed/clean_text_2024-09-16.csv"))

# drop any rows with NA for Value_Orientation
articles_text_clean <- articles_text_clean[!is.na(articles_text_clean$Value_Orientation), ]

# Load the unlabeled documents to make sure predict.model_fit() will work
unlabeled_articles <- read_csv(here::here("data/processed/newdata_for_model_testing_text_2024-11-06.csv"))
unlabeled_articles <- unlabeled_articles %>%
  dplyr::select(Article_Text)

# select relevant variables from articles_text_clean

text2wvo <- articles_text_clean %>%
  dplyr::select(Article_Text, Value_Orientation)

## make Value_Orientation a factor
text2wvo$Value_Orientation <- as.factor(text2wvo$Value_Orientation)

# then split the data into training and testing sets
text_split <- initial_split(text2wvo, strat = Value_Orientation)

text_train <- training(text_split)
text_test <- testing(text_split)

# create the text training recipe (model)
text_rec <- 
  recipe(Value_Orientation ~ Article_Text, data = text_train)

# tokenize the article text, set the max number of tokens as a tuneable parameter, create tfidf
## check with Lily, but I may want to clean the text in the same way (stop words, lemmatize, etc)
text_rec_v1 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_tokenfilter(Article_Text, 
                   max_tokens = tune(),
                   min_times = 100) %>%
  step_tfidf(Article_Text) 

text_rec_v2 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_lemma(Article_Text) %>% #optional, funny issue with this step, did not work
  #step_stopwords(Article_Text) %>% #optional
  step_tokenfilter(Article_Text, 
                   max_tokens = tune(),
                   min_times = 100) %>%
  step_tfidf(Article_Text) #%>%
#step_downsample(Value_Orientation) #optional, downscale for unbalanced data

text_rec_v3 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_stopwords(Article_Text) %>% #optional
  step_tokenfilter(Article_Text, 
                   max_tokens = tune(),
                   min_times = 100) %>%
  step_tfidf(Article_Text) #%>%
#step_downsample(Value_Orientation) #optional, downscale for unbalanced data

text_rec_v4 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_stopwords(Article_Text) %>% #optional
  step_tokenfilter(Article_Text, 
                   max_tokens = tune(),
                   min_times = 100) %>%
  step_tfidf(Article_Text) %>%
  step_downsample(Value_Orientation) #optional, downscale for unbalanced data

# create a simple linear model 
lasso_spec <- multinom_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# create a tuneable linear model
lasso_tune <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe(text_rec_v3) %>%
  add_model(lasso_spec)
lasso_wf

lasso_tune_wf <- workflow() %>%
  add_recipe(text_rec_v3) %>%
  add_model(lasso_tune)
lasso_tune_wf

# fit to the resampled data 
## create cross validation folds
set.seed(234)
text_folds <- vfold_cv(text_train)

## parameter tuning 
cntrl <- control_resamples(save_pred = TRUE)

tokens_tune_grid <- grid_regular(
  max_tokens(range = c(500, 1300)),
  levels = c(max_tokens = 9)
)
tokens_tune_grid

full_tune_grid <- grid_regular(
  max_tokens(range = c(500, 1300)),
  penalty(range = c(-4, 0)),
  levels = c(max_tokens = 5, penalty = 20)
)
full_tune_grid

# tune the model on the cv folds created from the training data
set.seed(2244)
lasso_rs <- tune_grid(
  lasso_wf, 
  resamples = text_folds, 
  grid = tokens_tune_grid,
  control = cntrl
)
lasso_rs

best_acc <- lasso_rs %>%
  show_best(metric = "accuracy")
best_acc

autoplot(lasso_rs)

lasso_rs %>%
  collect_predictions() %>%
  filter(max_tokens == best_acc$max_tokens) %>%
  filter(id == "Fold01") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap")

# tune both max tokens and penalty
lasso_tune_rs <- tune_grid(
  lasso_tune_wf, 
  resamples = text_folds, 
  grid = full_tune_grid,
  control = cntrl
)
lasso_tune_rs

best_tune_acc <- lasso_tune_rs %>%
  show_best(metric = "accuracy")
best_tune_acc

autoplot(lasso_tune_rs)

lasso_tune_rs %>%
  collect_predictions() %>%
  filter(max_tokens == best_tune_acc$max_tokens) %>%
  filter(penalty == best_tune_acc$penalty) %>%
  filter(id == "Fold01") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap")

lasso_final <- lasso_tune_wf %>%
  finalize_workflow(
    select_best(x = lasso_tune_rs, metric = "accuracy")
  )

lasso_final_rs <- fit_resamples(
  lasso_final, 
  text_folds, 
  control = control_resamples(save_pred = TRUE)
)
#autoplot(lasso_final_rs)

lasso_rs_metrics <- collect_metrics(lasso_final_rs)
lasso_rs_predictions <- collect_predictions(lasso_final_rs)

lasso_rs_predictions %>%
  filter(id == "Fold02") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap") 

lasso_final_fitted <- last_fit(lasso_final, text_split)

collect_metrics(lasso_final_fitted)

collect_predictions(lasso_final_fitted) %>%
  conf_mat(truth = Value_Orientation, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

lasso_final_wf <- lasso_final_fitted %>%
  extract_workflow()

predict(lasso_final_wf, new_data = unlabeled_articles)

# repeat the process with a knn model
# set up a knn workflow


knn_tune_grid <- grid_regular(
  max_tokens(range = c(500, 1300)),
  neighbors(range = c(2, 10)),
  levels = c(max_tokens = 5, neighbors = 9)
)
knn_tune_grid

# create a knn model 
knn_spec <- nearest_neighbor(neighbors = 5) %>%
  set_engine("kknn") %>%
  set_mode("classification")

# create a tuneable knn model
knn_tune <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_wf <- workflow() %>%
  add_recipe(text_rec_v3) %>%
  add_model(knn_spec)
knn_wf

knn_tune_wf <- workflow() %>%
  add_recipe(text_rec_v3) %>%
  add_model(knn_tune)
knn_tune_wf

set.seed(2244)
knn_rs <- tune_grid(
  knn_wf,
  resamples = text_folds,
  grid = tokens_tune_grid, 
  control = cntrl
)

autoplot(knn_rs)

knn_tune_rs <- tune_grid(
  knn_tune_wf,
  resamples = text_folds,
  grid = knn_tune_grid, 
  control = cntrl
)

autoplot(knn_tune_rs)

best_knn_tune_acc <- knn_tune_rs %>%
  show_best(metric = "accuracy")
best_knn_tune_acc

knn_final <- knn_tune_wf %>%
  finalize_workflow(
    select_best(x = knn_tune_rs, metric = "accuracy")
  )

knn_final_rs <- fit_resamples(
  knn_final, 
  text_folds, 
  control = control_resamples(save_pred = TRUE)
)

knn_rs_metrics <- collect_metrics(knn_final_rs)
knn_rs_predictions <- collect_predictions(knn_final_rs)

knn_rs_predictions %>%
  filter(id == "Fold02") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap") 

knn_final_fitted <- last_fit(knn_final, text_split)

collect_metrics(knn_final_fitted)

collect_predictions(knn_final_fitted) %>%
  conf_mat(truth = Value_Orientation, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

knn_final_wf <- knn_final_fitted %>%
  extract_workflow()

predict(knn_final_wf, new_data = unlabeled_articles)



