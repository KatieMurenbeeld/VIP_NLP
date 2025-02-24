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

text_wf <- workflow() %>%
  add_recipe(text_rec)

nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb_spec

nb_fit <- text_wf %>%
  add_model(nb_spec) %>%
  fit(data = text_train)

set.seed(234)
text_folds <- vfold_cv(text_train)

nb_wf <- workflow() %>%
  add_recipe(text_rec) %>%
  add_model(nb_spec)

nb_rs <- fit_resamples(
  nb_wf, 
  text_folds, 
  control = control_resamples(save_pred = TRUE)
)

nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_predictions <- collect_predictions(nb_rs)


nb_rs_predictions %>%
  filter(id == "Fold03") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap") 

final_fit <- last_fit(nb_wf, text_split)

final_wf <- final_fit %>%
  extract_workflow()

predict(final_wf, new_data = unlabeled_articles) #it worked!!

# this workflow worked. now I need to train and test knn, svm, and rf
# Tuning and training knn from 
# https://optimumsportsperformance.com/blog/k-nearest-neighbor-tidymodels-tutorial/

knn_spec <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_wf <- workflow() %>%
  add_recipe(text_rec) %>%
  add_model(knn_spec)

cntrl <- control_resamples(save_pred = TRUE)

knn_tune_grid <- grid_regular(
  neighbors(range = c(2, 10))
)

knn_tune <- tune_grid(
  knn_wf,
  resamples = text_folds,
  grid = knn_tune_grid, 
  control = cntrl
)
  
knn_tune %>% 
  collect_metrics()
  
best_neighbors <- knn_tune %>%
  select_best(metric = "accuracy") %>%
  pull(neighbors)

knn_final <- knn_wf %>%
  finalize_workflow(
    select_best(x = knn_tune, metric = "accuracy")
  )

knn_rs <- fit_resamples(
  knn_final, 
  text_folds, 
  control = control_resamples(save_pred = TRUE)
)

knn_rs_metrics <- collect_metrics(knn_rs)
knn_rs_predictions <- collect_predictions(knn_rs)

knn_rs_predictions %>%
  filter(id == "Fold10") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap") 

knn_final_fit <- last_fit(knn_final, text_split)

knn_final_wf <- knn_final_fit %>%
  extract_workflow()

predict(knn_final_wf, new_data = unlabeled_articles) #it worked!!
