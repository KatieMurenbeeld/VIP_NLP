#===============================================================================
# Comparing Non-ordinal and Ordinal Random Forest Classification Models
# 
# 1. Load the data and prep/bake/reformat
# Load the fit models from _forest_classification_testing.R
# Make predictions using the test data
# Compare the metrics and confusion matrices
### need to include the various kappa functions
#===============================================================================

# 0. Load the required packages
#-------------------------------------------------------------------------------
library(ordinalForest)
library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(tm)
library(caret)
library(stopwords)
library(future)
library(doParallel)
library(foreach)

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

# 1.2. Split the data into training and testing
#-------------------------------------------------------------------------------
set.seed(1986)
text_split <- initial_split(text2wvo, strat = Value_Orientation)

text_train <- training(text_split)
text_test <- testing(text_split)

# 1.3. Create the text training recipe (model)
#-------------------------------------------------------------------------------
text_rec <- 
  recipe(Value_Orientation ~ Article_Text, data = text_train)

text_rec_more_var <- 
  recipe(Value_Orientation ~ Article_Text, Focus, data = text_train)

# 1.4. Modify the text recipe with several preprocessing steps.
#-------------------------------------------------------------------------------
# 1.4.1. Tokenize the article text
# 1.4.2  Remove stopwords (from the stopwords library)
# 1.4.3. Set parameters for token filtering
# 1.4.4. Generate a dtm using tf-idf
# 1.4.5. Prep and bake the data
# 1.4.6. Format the baked data into a dataframe for use in ordfor

# 1.4.1-1.4.4
text_rec_v1 <- text_rec %>%
  step_tokenize(Article_Text) %>% # Tokenize the article text
  step_stopwords(Article_Text) %>% # Remove stopwords (from the stopwords library)
  #step_tokenfilter(Article_Text, max_tokens = tune(), min_times = 100) %>%
  step_tokenfilter(Article_Text, max_tokens = 1300) %>% # Set parameters for token filtering
  step_tfidf(Article_Text) #4 Generate a dtm using tf-idf

# 1.4.5. Prep and bake the data
text_obj <- prep(text_rec_v1) 
baked_data <- bake(text_obj, text_train)
baked_data_test <- bake(text_obj, text_test)

# 1.4.6. Format the data for use in ordfor
baked_dataframe <- as.data.frame(baked_data)
baked_test_dataframe <- as.data.frame(baked_data_test)

# 2. Load the previously fit forest models
#-------------------------------------------------------------------------------
## model created with non-ordinal RF, ntrees = 2000
rf_mod <- readRDS(here::here("output/models/rf_final_wf_2025-12-10.rds"))

## model created with ordinal RF, default params, perffunction = equal
of_def <- readRDS(here::here("output/ordforFitequal_2025-12-08.RDS"))

## model created with ordinal RF, default params, perffunction = probability
of_prob <- readRDS(here::here("output/ordforFitprob_2025-12-08.RDS"))

## model created with ordinal RF, default params, perffunction = proportional
of_propor <- readRDS(here::here("output/ordforFitpropor_2025-12-09.RDS"))

## model created with ordinal RF, default params, perffunction = oneclass, classimp = 1
of_oneclass <- readRDS(here::here("output/ordforFitoneclass_2025-12-09.RDS"))

## model created with ordinal RF, default params, perffunction = custom
## weights = [0.25, 0.1, 0.1, 0.1, 0.1, 0.1, 0.25]
of_custom <- readRDS(here::here("output/ordforFitcustom_2025-12-09.RDS"))

## model created with ordinal RF, tuned params, perffunction = equal
of_tune_def <- readRDS(here::here("output/ordforFit_tuneequal_2025-12-10.RDS"))

## model created with ordinal RF, tuned params, perffunction = probability
of_tune_prob <- readRDS(here::here("output/ordforFit_tuneprob_2025-12-10.RDS"))

## model created with ordinal RF, tuned params, perffunction = proportional
of_tune_propor <- readRDS(here::here("output/ordforFit_tunepropor_2025-12-10.RDS"))

## model created with ordinal RF, tuned params, perffunction = oneclass, classimp = 1
of_tune_oneclass <- readRDS(here::here("output/ordforFit_tuneoneclass_2025-12-10.RDS"))

## model created with ordinal RF, tuned params, perffunction = custom, 
## weights = [0.25, 0.1, 0.1, 0.1, 0.1, 0.1, 0.25]
of_tune_custom <- readRDS(here::here("output/ordforFit_tunecustom_2025-12-10.RDS"))

# 3. Use the fit models to predict the test data
#-------------------------------------------------------------------------------

## I think because of tidymodels I have to use the text_test data and not the 
## "baked" data for the rf model since it includes the text processing workflow.
## Also, need to get the labels from the test data

label_test <- as.factor(baked_data_test$Value_Orientation)

#rf_predict <- predict(rf_mod, text_test) 
rf_cm<- confusionMatrix(rf_predict$.pred_class, label_test, mode = "prec_recall")
rf_cm

of_def_pred <- predict(of_def, baked_test_dataframe)
of_def_pred_predicted <- of_def_pred$ypred

of_def_cm <- confusionMatrix(data = of_def_pred$ypred, reference = label_test)
of_def_cm
