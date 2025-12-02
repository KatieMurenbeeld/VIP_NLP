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
baked_data_test <- bake(text_obj, text_test)

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
## It doesn't seem like parsnip (which goes with tidymodels) has much 
## in the way of ordinal classification...
## Not sure how to do this with caret
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
  #set_engine("ranger") %>% # set probability = TRUE
  set_engine("ordinalRF") %>% 
  set_mode("classification")

##-----Trying this out with just caret------------------------------------------
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

ordforFit1 <- train(Value_Orientation ~., data = baked_data,
                    method = "ordinalRF", trControl = fitControl)
# save the fit model
saveRDS(ordforFit1, paste0(here::here("output/ordforFit1_", Sys.Date(), ".RDS")))

ordforFit1

ordforFit1$bestTune


ordforFit1_preds <- predict(ordforFit1, newdata = baked_data_test)
ordforFit1_true <- baked_data_test$Value_Orientation

confusionMatrix(data = ordforFit1_preds, reference = ordforFit1_true)

# Train the ordinalForest model with only the default values, no tuning
ordforFit2 <- train(Value_Orientation ~., data = baked_data,
                    method = "ordinalRF")

# save the fit model
saveRDS(ordforFit2, here::here(paste0("output/ordforFit2_", Sys.Date(), ".RDS")))

ordforFit2_preds <- predict(ordforFit2, newdata = baked_data_test)
ordforFit2_true <- baked_data_test$Value_Orientation

confusionMatrix(data = ordforFit2_preds, reference = ordforFit2_true)

# NOTE: ordfor() does not accept a tibble.
# No training or tuning, just the default parameters
baked_dataframe <- as.data.frame(baked_data)
head(baked_dataframe)

baked_test_dataframe <- as.data.frame(baked_data_test)

ordforFit3 <- ordfor(depvar = "Value_Orientation", data = baked_dataframe)

# save the fit model
saveRDS(ordforFit3, here::here(paste0("output/ordforFit3_", Sys.Date(), ".RDS")))

ordforFit3_preds <- predict(ordforFit3, newdata = baked_test_dataframe)
ordforFit3_predicted <- ordforFit3_preds$ypred
ordforFit3_true <- baked_data_test$Value_Orientation

confusionMatrix(data = ordforFit3_predicted, reference = ordforFit3_true)


# kappa functions from Hornung, 2020
unweightedkappa <- function(ytrue, yhat) {
  
  require("psych")
  
  x <- data.frame(ytrue=ytrue, yhat=yhat)
  
  cohen.kappa(x)$kappa
  
}

linearkappa <- function(ytrue, yhat) {
  
  require("psych")
  
  x <- data.frame(ytrue=ytrue, yhat=yhat)
  
  J <- length(unique(c(x$ytrue, x$yhat)))
  
  myw <- matrix(0, ncol = J, nrow = J)
  myw[] <- abs((col(myw) - row(myw)))
  myw <- 1 - myw/(J - 1)
  
  cohen.kappa(x, w=myw)$weighted.kappa
  
}

quadratickappa <- function(ytrue, yhat) {
  
  require("psych")
  
  x <- data.frame(ytrue=ytrue, yhat=yhat)
  
  J <- length(unique(c(x$ytrue, x$yhat)))
  
  myw <- matrix(0, ncol = J, nrow = J)
  myw[] <- abs((col(myw) - row(myw)))^2
  myw <- 1 - myw/(J - 1)^2
  
  cohen.kappa(x, w=myw)$weighted.kappa
  
}

fit1_kappa <- unweightedkappa(ordforFit1_true, ordforFit1_preds)
fit2_kappa <- unweightedkappa(ordforFit2_true, ordforFit2_preds)
fit3_kappa <- unweightedkappa(ordforFit3_true, ordforFit3_predicted)

fit1_kappalin <- linearkappa(ordforFit1_true, ordforFit1_preds)
fit2_kappalin <- linearkappa(ordforFit2_true, ordforFit2_preds)
fit3_kappalin <- linearkappa(ordforFit3_true, ordforFit3_predicted)

fit1_kappaqu <- quadratickappa(ordforFit1_true, ordforFit1_preds)
fit2_kappaqu <- quadratickappa(ordforFit2_true, ordforFit2_preds)
fit3_kappaqu <- quadratickappa(ordforFit3_true, ordforFit3_predicted)

# CART

cartFit1 <- train(Value_Orientation ~., data = baked_data,
                  method = "rpartScore", trControl = fitControl)

