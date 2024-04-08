library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(discrim)
library(naivebayes)
library(recipes)
library(hardhat)
library(glmnet)

# Load the cleaned data
articles_text_clean <- read_csv(here::here("data/processed/grizz_clean_text_2024-04-04.csv"))

## Clean up multiple-spellings in species names, focus, and conflict type
articles_text_clean$Conflict_Type <- str_replace(articles_text_clean$Conflict_Type, 'N-W', 'Nature-Wildlife')

## A few articles have a bunch of html code at the end so remove that
### may want to add this as an if-else to clean up the larger corpus
articles_text_clean[[5]][15] <- str_extract(articles_text_clean[[5]][15], regex(".+?(?=</div>)"))

# Split the data
grizz_split <- initial_split(articles_text_clean, strata = Focus)

grizz_train <- training(grizz_split)
grizz_test <- testing(grizz_split)

# Tokenize and create dtm
## Tokenize
tidy_grizz_train <- grizz_train %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word))  

tidy_grizz_test <- grizz_test %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word))  

## Check the most common words
tidy_grizz_train %>%
  count(word, sort = TRUE) 

tidy_grizz_test %>%
  count(word, sort = TRUE)

## Remove stop words
## Create a small data frame of your own stop words for this project 
data("stop_words")
grizz_stop_words <- data.frame(c("p", "br", "strong", "targetednews.com",
                                 "grizzly", "grizzlies", "bears", "bear")) 
colnames(grizz_stop_words) <-("word")

tidy_grizz_train <- tidy_grizz_train %>%
  anti_join(stop_words) %>%
  anti_join(grizz_stop_words)

tidy_grizz_test <- tidy_grizz_test %>%
  anti_join(stop_words) %>%
  anti_join(grizz_stop_words)

tidy_grizz_train <- tidy_grizz_train %>%
  select(Focus, Conflict_Type, Value_Orientation, Publication_State, word)

tidy_grizz_test <- tidy_grizz_test %>%
  select(Focus, Conflict_Type, Value_Orientation, Publication_State, word)

tidy_grizz_test %>%
  count(Focus, word, sort = TRUE)

#---Ignore the below for now-------------------------------------
# Split the data
grizz_split <- initial_split(articles_text_clean, strata = Focus)

grizz_train <- training(grizz_split)
grizz_test <- testing(grizz_split)

#---Check the number of cases in each Focus type
# Remeber articles can have more than one Focus
grizz_train %>%
  count(Focus, sort = TRUE) %>%
  select(n, Focus)


#---Create the recipe and workflow--
# start with the formula expression
grizz_rec <- 
  recipe(Focus ~ Article_Text, data = grizz_train)

# next add processing steps
grizz_rec <- grizz_rec %>%
  step_tokenize(Article_Text) %>%
  step_tokenfilter(Article_Text, max_tokens = 1e3) %>%
  step_tfidf(Article_Text)

#---Build a lasso regularization
multi_spec <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

multi_spec

# setup a sparse blueprint, forces data to be processes as sparse data
sparse_bp <- hardhat::default_formula_blueprint(composition = "dgCMatrix")

# Create the workflow 
grizz_wf <- workflow() %>%
  #add_recipe(grizz_rec, blueprint = sparse_bp) %>% # error because the blueprint class doesn't match the required blueprint class for some reason
  add_recipe(grizz_rec) %>%
  add_model(multi_spec)

#grizz_sparse_wf <- grizz_wf %>%
#  update_recipe(grizz_rec, blueprint = sparse_bp)

#---Model Evaluation--

# create 10-fold cross-validation

set.seed(234)

grizz_folds <- vfold_cv(grizz_train)
grizz_folds

# fit to the resampled data
smaller_lambda <- grid_regular(penalty(range = c(-5, 0)), levels = 20)

mod_rs <- tune_grid(
  grizz_wf,
  grizz_folds,
  grid = smaller_lambda,
  control = control_resamples(save_pred = TRUE)
)

mod_rs

mod_rs_metrics <- collect_metrics(mod_rs)
mod_rs_preditions <- collect_predictions(mod_rs)




