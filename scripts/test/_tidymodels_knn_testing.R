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

tdm_articles <- read_csv(here::here("data/original/grizzly_bear_corpus_gamma_threshold_0.5.csv"))
tdm_articles <- tdm_articles %>%
  dplyr::select(Text) %>%
  rename(Article_Text = Text)


text2wvo <- articles_text_clean %>%
  dplyr::select(Article_Text, Value_Orientation)

text2wvo$Value_Orientation <- as.factor(text2wvo$Value_Orientation)
hist(as.numeric(text2wvo$Value_Orientation))

text_split <- initial_split(text2wvo, strat = Value_Orientation)

text_train <- training(text_split)
text_test <- testing(text_split)


text_rec <- 
  recipe(Value_Orientation ~ Article_Text, data = text_train)

text_rec_v1 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_tokenfilter(Article_Text, max_tokens = tune(), min_times = 100) %>%
  step_tfidf(Article_Text) %>%
  step_downsample(Value_Orientation) #downscale for unbalanced data

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

knn_tune_grid <- grid_regular(
  neighbors(range = c(2, 10))
)

final_tune_grid <- grid_regular(
  neighbors(range = c(2, 10)), 
  #max_tokens(range = c(1000, 3000)),
  max_tokens(range = c(500, 1300)),
  levels = c(neighbors = 9, max_tokens = 9)
)
final_tune_grid

## tuneable knn model
knn_spec <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

## create workflow with tuneable model
knn_tune_wf <- workflow() %>%
  add_recipe(text_rec_v1) %>%
  add_model(knn_spec)

set.seed(2244)
knn_tune_rs <- tune_grid(
  knn_tune_wf,
  resamples = text_folds,
  grid = final_tune_grid, 
  control = cntrl
)

autoplot(knn_tune_rs)

knn_tune_rs %>%
  collect_predictions() %>%
  filter(id == "Fold02") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap") 


best_neighbors <- knn_tune_rs %>%
  select_best(metric = "accuracy") %>%
  pull(neighbors)

best_tokens <- knn_tune_rs %>%
  select_best(metric = "accuracy") %>%
  pull(max_tokens)

#somewhere need to update the text_rec with the best max tokens
# max_tokens = 500 and neighbors = 5

knn_final <- knn_tune_wf %>%
  finalize_workflow(
    select_best(x = knn_tune_rs, metric = "accuracy")
  )

knn_final

knn_rs <- fit_resamples(
  knn_final, 
  text_folds, 
  control = control_resamples(save_pred = TRUE)
)

knn_rs_metrics <- collect_metrics(knn_rs)
knn_rs_predictions <- collect_predictions(knn_rs)

knn_rs_predictions %>%
  filter(id == "Fold02") %>%
  conf_mat(truth = Value_Orientation, .pred_class) %>%
  autoplot(type = "heatmap") 


knn_final_fit <- last_fit(knn_final, text_split)

knn_final_wf <- knn_final_fit %>%
  extract_workflow()

test_final_wf <- knn_final_fit %>%
  extract_fit_parsnip()

predict(knn_final_wf, new_data = tdm_articles) #it worked!! with both my test "unlabeled_articles" and the "tdm_articles"

# So I at least know that I can use this code with the csvs that Lily made on TDM
write_rds(knn_final_wf, file = here::here("output/models/test_final_wf.rds"))
test_wf <- readRDS(here::here("output/models/test_final_wf.rds"))
test_wf
predict(test_wf, new_data = unlabeled_articles)
# I should be able to save as an rds, load to tdm and then use it there. 

## Add a document id column
tdm_articles$id <- seq.int(nrow(tdm_articles))

tidy_text_unlabel <- tdm_articles %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word)) 

dtm_test <- tidy_text_unlabel %>%
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)

dtm_test_df <- dtm_test %>% as.matrix() %>% as.data.frame() 
predict(knn_final_wf, new_data = dtm_test_df) # i can't use a premade dtm. Tidymodels wants to go through the entire process. 

# maybe if I make a new workflow? But I might need to use the same work flow to train the model...








