#===============================================================================
# Testing and Comparing Random Forest Classification Models
# 
#
#
#
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
set.seed(1986)
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
# 4. Prep and bake the data

text_rec_v1 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_stopwords(Article_Text) %>%
  #step_tokenfilter(Article_Text, max_tokens = tune(), min_times = 100) %>%
  step_tokenfilter(Article_Text, max_tokens = 1300) %>%
  step_tfidf(Article_Text)

text_obj <- prep(text_rec_v1)
baked_data <- bake(text_obj, text_train)
baked_data_test <- bake(text_obj, text_test)

# 5. From the training data split, create the cross validation resampling folds for training/tuning
#-------------------------------------------------------------------------------

set.seed(234)
text_folds <- vfold_cv(text_train)

# For resamples, save the predictions
cntrl <- control_resamples(save_pred = TRUE)

# 6. Create a simple tuning grid to find the optimal number of tokens to retain 
# for model training (not sure if I want to keep this step)
#-------------------------------------------------------------------------------

tokens_tune_grid <- grid_regular(
  max_tokens(range = c(500, 1300)),
  levels = c(max_tokens = 9)
)
tokens_tune_grid

# 7. Random Forest Model (non ordinal)
#-------------------------------------------------------------------------------
# For this section we can use tidymodels (parsnip)

## 7.1 Set up a tuning grid
rf_tune_grid <- grid_regular(
  trees(range = c(1000, 5000)), 
  #max_tokens(range = c(500, 1300)),
  levels = c(trees = 5, 
             #max_tokens = 9
             )
)

## 7.2 Create the random forest model and workflow
rf_spec <- rand_forest(mtry = 20, 
trees = tune(),
min_n = 4
) %>%
  set_engine("ranger", probability = TRUE) %>% # set probability = TRUE
  set_mode("classification")

## create workflow with tuneable model
rf_tune_wf <- workflow() %>%
  add_recipe(text_rec_v1) %>%
  add_model(rf_spec)

## 7.3 Tune the model
set.seed(6891)
rf_tune_rs <- tune_grid(
  rf_tune_wf,
  resamples = text_folds,
  grid = rf_tune_grid, 
  control = cntrl
)

### Collect the accuracy results into a table from which you can select 
### the best number of trees to use in the final model
rf_best_acc <- rf_tune_rs %>%
  show_best(metric = "accuracy")

## 7.4 Create a final workflow with the optimized parameters
trees_use <- rf_best_acc$trees

rf_spec <- rand_forest(trees = trees_use[1]
) %>%
  set_engine("ranger", probability = TRUE) %>%
  set_mode("classification")

rf_final_wf <- workflow() %>%
  add_recipe(text_rec_v1) %>%
  add_model(rf_spec)
rf_final_wf

## 7.5 Fit the resamples
rf_final_rs <- fit_resamples(
  rf_final_wf, 
  text_folds, 
  control = control_resamples(save_pred = TRUE)
)

## 7.6 Complete the final fit of the model and save the workflow as a RDS object
rf_final_fitted <- last_fit(rf_final_test, text_split)

rf_final_wf <- rf_final_fitted %>%
  extract_workflow()

write_rds(rf_final_wf, file = here::here(paste0("output/models/rf_final_wf_",
                                                Sys.Date(), ".rds")))

# 8. Begin testing the ordinal forest
#-------------------------------------------------------------------------------

## This is the same cross validation as used for the non-ordinal RF
## NOT SURE IF I WILL DO ANY TRAINING?
set.seed(4433)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated one times
  repeats = 1)

## 8.1 Only the default parameters. perffunction = default ("equal")
ordforFitequal <- ordfor(depvar = "Value_Orientation", data = baked_dataframe)

# save the fit model
saveRDS(ordforFitequal, here::here(paste0("output/ordforFitequal_", Sys.Date(), ".RDS")))

## 8.2 The default parameters but the perffunction = "probability"
ordforFitprob <- ordfor(depvar = "Value_Orientation", data = baked_dataframe,
                     perffunction = "probability")

# save the fit model
saveRDS(ordforFitprob, here::here(paste0("output/ordforFitprob_", Sys.Date(), ".RDS")))

## 8.3 The default parameters but the perffunction = "proportional"
ordforFitpropor <- ordfor(depvar = "Value_Orientation", data = baked_dataframe,
                     perffunction = "probability")

# save the fit model
saveRDS(ordforFitpropor, here::here(paste0("output/ordforFitpropor_", Sys.Date(), ".RDS")))

## 8.4 The default parameters but the perffunction = "oneclass"
ordforFitoneclass <- ordfor(depvar = "Value_Orientation", data = baked_dataframe,
                     perffunction = "oneclass")

# save the fit model
saveRDS(ordforFitoneclass, here::here(paste0("output/ordforFitoneclass_", Sys.Date(), ".RDS")))

## 8.5 The default parameters but the perffunction = "custom"
ordforFitcustom <- ordfor(depvar = "Value_Orientation", 
                     data = baked_dataframe,
                     perffunction = "custom", # will need to define weight for each class 
                     classweights = c()) 

# save the fit model
saveRDS(ordforFitcustom, here::here(paste0("output/ordforFitcustom_", Sys.Date(), ".RDS")))

## 8.6 Tune/train the model and the perffunction = default
ordforTrain <- train(Value_Orientation ~., data = baked_data,
                    method = "ordinalRF")

### Save the best fit parameters (not sure how to set the perffunction when training with caret,
### so I'm not sure if these parameters are appropriate to use when perffunction is changed) 
OF_best <- ordforTrain$bestTune

## 8.7 Use the best fit parameters in the ordfor() with perffunction = "default"
ordforFit_tuneequal <- ordfor(depvar = "Value_Orientation", data = baked_dataframe,
                     nsets = OF_best$,
                     ntreeperdiv = OF_best$,
                     ntreefinal = OF_best$)

# save the fit model
saveRDS(ordforFit_tuneequal, here::here(paste0("output/ordforFit_tuneequal_", Sys.Date(), ".RDS")))

## 8.8 Use the best fit parameters in the ordfor() with perffunction = "probability"
ordforFit_tuneprob <- ordfor(depvar = "Value_Orientation", data = baked_dataframe,
                     perffunction = "probability",
                     nsets = OF_best$,
                     ntreeperdiv = OF_best$,
                     ntreefinal = OF_best$)

# save the fit model
saveRDS(ordforFit_tuneprob, here::here(paste0("output/ordforFit_tuneprob_", Sys.Date(), ".RDS")))

## 8.9 Use the best fit parameters in the ordfor() with perffunction = "proportional"
ordforFit_tunepropor <- ordfor(depvar = "Value_Orientation", data = baked_dataframe,
                     perffunction = "proportional",
                     nsets = OF_best$,
                     ntreeperdiv = OF_best$,
                     ntreefinal = OF_best$)

# save the fit model
saveRDS(ordforFit_tunepropor, here::here(paste0("output/ordforFit_tunepropor_", Sys.Date(), ".RDS")))

## 8.10 Use the best fit parameters in the ordfor() with perffunction = "oneclass"
ordforFit_tuneoneclass <- ordfor(depvar = "Value_Orientation", data = baked_dataframe,
                     perffunction = "oneclass",
                     nsets = OF_best$,
                     ntreeperdiv = OF_best$,
                     ntreefinal = OF_best$)

# save the fit model
saveRDS(ordforFit_tuneoneclass, here::here(paste0("output/ordforFit_tuneoneclass_", Sys.Date(), ".RDS")))

## 8.11 Use the best fit parameters in the ordfor() with perffunction = "custom"
ordforFit_tunecustom <- ordfor(depvar = "Value_Orientation", data = baked_dataframe,
                      perffunction = "custom", # will need to define weight for each class 
                      classweights = c(), 
                     nsets = OF_best$,
                     ntreeperdiv = OF_best$,
                     ntreefinal = OF_best$)

# save the fit model
saveRDS(ordforFit_tunecustom, here::here(paste0("output/ordforFit_tunecustom_", Sys.Date(), ".RDS")))





