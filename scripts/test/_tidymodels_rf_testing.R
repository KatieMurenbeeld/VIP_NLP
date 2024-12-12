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

text_rec_v1 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_tokenfilter(Article_Text, max_tokens = 1000) %>%
  step_tfidf(Article_Text)
#step_downsample(Value_Orientation) #downscale for unbalanced data

text_rec_v2 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_tokenfilter(Article_Text, 
                   max_tokens = tune(),
                   min_times = 100) %>%
  step_tfidf(Article_Text)

set.seed(234)
text_folds <- vfold_cv(text_train)

text_wf_v1 <- workflow() %>%
  add_recipe(text_rec_v1)

text_wf_v2 <- workflow() %>%
  add_recipe(text_rec_v2)

# parameter tuning 
cntrl <- control_resamples(save_pred = TRUE)
rf_tune_grid <- grid_regular(
  trees(range = c(1000, 2000))
)

final_tune_grid <- grid_regular(
  trees(range = c(1000, 5000)), 
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  max_tokens(range = c(500, 1300)),
  levels = c(trees = 5, 
             mtry = 5, 
             min_n = 5, 
             max_tokens = 9)
)
final_tune_grid

rf_spec <- rand_forest(mtry = 500, 
                       trees = tune()
                       #min_n = tune()
                       ) %>%
  set_engine("ranger") %>%
  set_mode("classification")

## create workflow with tuneable model
rf_tune_wf <- workflow() %>%
  add_recipe(text_rec_v1) %>%
  add_model(rf_spec)
rf_tune_wf

set.seed(6891)
rf_tune_rs <- tune_grid(
  rf_tune_wf,
  resamples = text_folds,
  grid = rf_tune_grid, 
  control = cntrl
)

autoplot(rf_tune_rs)

rf_tune_rs %>%
  collect_predictions() %>%
  filter(id == "Fold02") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap") 

best_trees <- rf_tune_rs %>%
  select_best(metric = "accuracy") %>%
  pull(neighbors)

#somewhere need to update the text_rec with the best max tokens
# max_tokens = 500 and neighbors = 5

rf_final <- rf_tune_wf %>%
  finalize_workflow(
    select_best(x = rf_tune_rs, metric = "accuracy")
  )

rf_final

rf_rs <- fit_resamples(
  rf_final, 
  text_folds, 
  control = control_resamples(save_pred = TRUE)
)

rf_rs_metrics <- collect_metrics(rf_rs)
rf_rs_predictions <- collect_predictions(rf_rs)

rf_rs_predictions %>%
  filter(id == "Fold02") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap") 
