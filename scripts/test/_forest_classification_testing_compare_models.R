#===============================================================================
# Comparing Non-ordinal and Ordinal Random Forest Classification Models
# 
# 1. Load the data and prep/bake/reformat
# 2. Load the fit models from _forest_classification_testing.R
# 3. Make predictions using the test data
# 4. Kappa functions from Hornung, 2020
# 5. Compare the metrics and confusion matrices
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
library(reshape2)

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

# 1.4.6. Format the data for use in ordfor which does not accept tibbles
baked_dataframe <- as.data.frame(baked_data)
baked_test_dataframe <- as.data.frame(baked_data_test)

# 2. Load the previously fit forest models
#-------------------------------------------------------------------------------

## Get a list of all RDS model file paths in the forest_models directory
file_paths <- list.files(path = here::here("output/forest_models"),
                         pattern = "\\.RDS$", full.names = TRUE)

## Read each file into a list element using readRDS
data_list <- lapply(file_paths, readRDS)

## Get the file names without the paths or the date and extension
pattern <- "(?<=models/).*?(?=_2025)"
file_names <- stringr::str_extract(file_paths, pattern)

## Assign these names to the list elements
names(data_list) <- file_names
#file_names 

# 3. Use the fit models to predict the test data
#-------------------------------------------------------------------------------
## Set the future::plan to work with multiple cores for speed
future::plan(future::multisession(workers = 3))

## Create an object with the test data labels
label_test <- as.factor(baked_data_test$Value_Orientation)

## Because the rf_final_wf model was made with tidymodels you have to use the  
## text_test data and not the "baked" test data when predicting.

## Use lapply to create a list of results for the one random forest model
rf_list <- lapply(data_list[11], function(x) { # for the rf_final_wf
  # first, get the predictions
  predictions <- predict(x, text_test)
  preds <- predictions$.pred_class
  # then, generate the confusion matrix
  cm <- confusionMatrix(data = predictions$.pred_class,
                        reference = label_test,
                        mode = "everything")
})

## The rest of the models can be predicted with the baked_test_dataframe
## Use lapply to create a list of results for each of the ordinal forest models
result_list <- lapply(data_list[1:10], function(x) { # ignore the rf_final_wf
  # first, get the predictions
  predictions <- predict(x, baked_test_dataframe)
  # then, generate the confusion matrix
  cm <- confusionMatrix(data = predictions$ypred,
                  reference = label_test,
                  mode = "everything")
})
#print(result_list)

# Append the rf_list to the result_list
result_list <- append(result_list, rf_list)

# 5. Create heat maps of the confusion matrices to compare the different models
#-------------------------------------------------------------------------------
## Create heat maps of the confusion matrix for each tested model
### Create a function that takes in the model name as an argument
### Then selects and reshapes the confusion matrix table
### And plots a heat map of the Reference (true) and Predicted class
heatmap.func <- function(model) {
  # Take in the model name
  x <- melt(result_list[[model]][2]) # reshape the confusion matrix table
  ggplot(data = x, aes(x = Reference, y = Prediction, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "white", high = "blue", midpoint = mean(x$value)) +
    geom_text(aes(label = value), vjust = 0.5, color = "black", size = 4) + # Add text labels
    scale_y_reverse(breaks = c(1:7)) + # 
    scale_x_continuous(breaks = c(1:7), position = "top") +
    labs(x = "Actual Class", y = "Predicted Class", fill = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),  # Center the title
          panel.grid.major = element_blank(), # Remove major gridlines
          panel.grid.minor = element_blank(), # Remove minor gridlines
          legend.position = "None") + # Remove the legend
    ggtitle(paste(model, "Confusion Matrix")) # Add the model name to the title
}

## Use a for loop to run through each model name and confusion matrix table
## within the result_list
for (i in 1:11) {
  # print the model name as a check
  print(names(result_list[i])) 
  # plot the heat map using the heatmap.func()
  plot(heatmap.func(names(result_list[i]))) 
  # save each plot to the output/plots directory
  ggsave(here::here(paste0("output/plots/", names(result_list[i]), "_confusion_matrix_", 
                           Sys.Date(), ".jpeg")),
         height = 5, width = 5, dpi = 300)
}



# 6. Combine all accuracy and kappa measures into one table for comparison
#-------------------------------------------------------------------------------
# get the overall metrics from the results_list
# get the by class metrics from the results list
# probably a bit of reshaping will be required
# Final table(s) should be:
# Model Name | Overall Accuracy | Kappa | LW Kappa | QW Kappa | By Class Metrics?


## 6.1 Get the overall metrics
### for these kappa functions I need the true data which is baked_data_test$Value_Orientation
### and the predicted data which is the model_preds$ypreds

rf_preds_list <- lapply(data_list[11], function(x) { # for the rf_final_wf
  # first, get the predictions
  predictions <- predict(x, text_test)
  preds <- predictions$.pred_class
})

model_preds_list <- lapply(data_list[1:10], function(x) { # for the rf_final_wf
  # first, get the predictions
  predictions <- predict(x, baked_test_dataframe)
  preds <- predictions$ypred
})

### Append the rf_preds_list to the model_preds_list
model_preds_list <- append(model_preds_list, rf_preds_list)

## Call the Kappa functions
source(here::here("functions/hornung_kappas.R"))

## Create an empty list
metrics_list <- list()

## Use a for loop to append the metrics to the empty dataframe 
for (i in 1:11) {
  # print the model name as a check
  name <- names(result_list[i])
  tmp_df <- as.data.frame(unlist(result_list[[name]][3]))
  acc <- tmp_df["overall.Accuracy", ]
  kap <- tmp_df["overall.Kappa", ]
  
  model_pred <- model_preds_list[i]
  lw <- linearkappa(label_test, model_pred)
  qw <- quadratickappa(label_test, model_pred)
  
  new_list <- list(
    model_name = name,
    overall_accuracy = acc, 
    kappa = kap, 
    lw_kappa = lw, 
    qw_kappa = qw
  )
  
  # Append the results to the list
  metrics_list[[i]] <- new_list
}
# bind the rows of the list created with the for loop into a dataframe
metric_df <- bind_rows(metrics_list) 

write_csv(metric_df, here::here(paste0("output/forest_models/model_metric_comparisons_",
                                       Sys.Date(), ".csv")))

## 6.2 Get the by class metrics?
