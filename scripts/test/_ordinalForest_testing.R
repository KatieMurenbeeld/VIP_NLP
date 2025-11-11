library(ordinalForest)
library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(tm)
library(caret)
library(stopwords)


# 1. Load the data
#-------------------------------------------------------------------------------
# Load the cleaned data
articles_text_clean <- read_csv(here::here("data/processed/clean_text_2024-09-16.csv"))

# drop any rows with NA for Value_Orientation
articles_text_clean <- articles_text_clean[!is.na(articles_text_clean$Value_Orientation), ]

# select relevant variables from articles_text_clean
text2wvo <- articles_text_clean %>%
  dplyr::select(Article_Text, Value_Orientation, Focus)

## make Value_Orientation a factor
text2wvo$Value_Orientation <- as.factor(text2wvo$Value_Orientation)

head(text2wvo)


# 2. Split the data into training and testing
#-------------------------------------------------------------------------------

text_split <- initial_split(text2wvo, strat = Value_Orientation)

text_train <- training(text_split)
text_test <- testing(text_split)

# 3. Create the text training recipe (model)
#-------------------------------------------------------------------------------
text_rec <- 
  recipe(Value_Orientation ~ Article_Text, data = text_train)

text_rec_more_var <- 
  recipe(Value_Orientation ~ Article_Text, Focus, data = text_train)

# 4. Modify the text recipe with several preprocessing steps.
#-------------------------------------------------------------------------------
# 1.  Tokenize the article text
# 2.  Remove stopwords (from the stopwords library)
# 3.  Set parameters for token filtering

text_rec_v1 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_stopwords(Article_Text) %>%
  #step_tokenfilter(Article_Text, max_tokens = tune(), min_times = 100) %>%
  step_tokenfilter(Article_Text, max_tokens = 1300) %>%
  step_tfidf(Article_Text)

text_obj <- prep(text_rec_v1)
baked_data <- bake(text_obj, text_train)


# 5. From the training data split, create the cross validation resampling folds for training/tuning
#-------------------------------------------------------------------------------

set.seed(234)
text_folds <- vfold_cv(text_train)

# For resamples, save the predictions
cntrl <- control_resamples(save_pred = TRUE)

# 6. Create a simple tuning grid to find the optimal number of tokens to retain for model training
#-------------------------------------------------------------------------------

tokens_tune_grid <- grid_regular(
  max_tokens(range = c(500, 1300)),
  levels = c(max_tokens = 9)
)
tokens_tune_grid

# 7. Create the ordinal forest model
## I want to test a few including the ordinalForest and also the rpartScore
## There are quite a few ordinal options to try out in the caret package
#-------------------------------------------------------------------------------
# create the model 
ordrf_spec <- ordfor(data = baked_data,
                     depvar = "Value_Orientation",
                     nsets = 1000, 
                     ntreeperdiv = 100,
                     ntreefinal = 5000,
                     importance = "rps",
                     perffunction = "probability"
) %>%
  #set_engine("ranger") %>%
  set_engine("ordinalRF") %>% # set probability = TRUE
  set_mode("classification")
