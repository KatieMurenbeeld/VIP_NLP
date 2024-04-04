library(tidyverse)
library(tidymodels)
library(textrecipes)
library(discrim)
library(naivebayes)

# Load the cleaned data
articles_text_clean <- read_csv(here::here("data/processed/grizz_clean_text_2024-04-04.csv"))

# Split the data
grizz_split <- initial_split(articles_text_clean, strata = Focus)

grizz_train <- training(grizz_split)
grizz_test <- testing(grizz_split)

#---Create the recipe and workflow----
# start with the formula expression
grizz_rec <- 
  recipe(Focus ~ Article_Text, data = grizz_train)

# next add processing steps
grizz_rec <- grizz_rec %>%
  step_tokenize(Article_Text) %>%
  step_tokenfilter(Article_Text, max_tokens = 1e3) %>%
  step_tfidf(Article_Text)

# create the workflow
grizz_wf <- workflow() %>%
  add_recipe(grizz_rec)

#---Build a naive Bayes model----
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb_spec

# fit the model
nb_fit <- grizz_wf %>%
  add_model(nb_spec) %>%
  fit(data = grizz_train)

#---Model Evaluation----

# create 10-fold cross-validation

set.seed(234)

grizz_folds <- vfold_cv(grizz_train)
grizz_folds

# create a workflow
nb_wf <- workflow() %>%
  add_recipe(grizz_rec) %>%
  add_model(nb_spec)

nb_wf

# fit to the resampled data
nb_rs <- fit_resamples(
  nb_wf,
  grizz_folds,
  control = control_resamples(save_pred = TRUE)
)

nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_preditions <- collect_predictions(nb_rs)




