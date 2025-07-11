library(tidymodels)
library(tidyverse)
library(tidytext)
library(textrecipes)
library(discrim)
library(themis)
library(recipes)
library(hardhat)
library(glmnet)
library(caret)
library(kknn)
library(ranger)
library(stopwords)
library(here)


# 1. Load the grizzly bear articles
#----

gbear_05 <- read_csv("~/SageMaker/SPWCM/grizzly_bear/data_2/final/final_corpus_gamma_0.5.csv")
gbear_055 <- read_csv("~/SageMaker/SPWCM/grizzly_bear/data_2/final/final_corpus_gamma_0.55.csv")
gbear_06 <- read_csv("~/SageMaker/SPWCM/grizzly_bear/data_2/final/final_corpus_gamma_0.6.csv")
gbear_065 <- read_csv("~/SageMaker/SPWCM/grizzly_bear/data_2/final/final_corpus_gamma_0.65.csv")

## Select and rename the article_text variable in order to use it with the trained model

gbear_05_use <- gbear_05 %>%
  dplyr::select(Text) %>%
  rename(Article_Text = Text)

gbear_055_use <- gbear_055 %>%
  dplyr::select(Text) %>%
  rename(Article_Text = Text)

gbear_06_use <- gbear_06 %>%
  dplyr::select(Text) %>%
  rename(Article_Text = Text)

gbear_065_use <- gbear_065 %>%
  dplyr::select(Text) %>%
  rename(Article_Text = Text)

# 2. Load the trained models
#----

reg_mod_wf <- readRDS("reg_final_wf.rds") # regression model workflow
knn_mod_wf <- readRDS("knn_final_wf.rds") # k nearest neighbors model workflow
rf_mod_wf <- readRDS("rf_final_wf.rds") # random forest model workflow

# 3. Predict!
#----

## 0.5 gamma threshold
reg_05_pred <- predict(reg_mod_wf, new_data = gbear_05_use)
knn_05_pred <- predict(knn_mod_wf, new_data = gbear_05_use)
rf_05_pred <- predict(rf_mod_wf, new_data = gbear_05_use)

## 0.55 gamma threshold
reg_055_pred <- predict(reg_mod_wf, new_data = gbear_055_use)
knn_055_pred <- predict(knn_mod_wf, new_data = gbear_055_use)
rf_055_pred <- predict(rf_mod_wf, new_data = gbear_055_use)

## 0.6 gamma threshold
reg_06_pred <- predict(reg_mod_wf, new_data = gbear_06_use)
knn_06_pred <- predict(knn_mod_wf, new_data = gbear_06_use)
rf_06_pred <- predict(rf_mod_wf, new_data = gbear_06_use)

## 0.65 gamma threshold
reg_065_pred <- predict(reg_mod_wf, new_data = gbear_065_use)
knn_065_pred <- predict(knn_mod_wf, new_data = gbear_065_use)
rf_065_pred <- predict(rf_mod_wf, new_data = gbear_065_use)

# 4. Rename the columns, combine the results, and save as csv
#----

## 0.5 gamma threshold
reg_05_pred_rename <- reg_05_pred %>%
  rename(reg_05_pred_class = .pred_class) # .pred_class is the default variable name output from predict()

knn_05_pred_rename <- reg_05_pred %>%
  rename(knn_05_pred_class = .pred_class)

rf_05_pred_rename <- reg_05_pred %>%
  rename(rf_05_pred_class = .pred_class)

## remove the Text column (cannot export from TDM)
gbear_05_notext <- gbear_05 %>%
  select(-Text)

## combine into one dataframe
gbear_05_preds <- cbind(gbear_05_notext, reg_05_pred_rename, knn_05_pred_rename, rf_05_pred_rename)

## 0.55 gamma threshold
reg_055_pred_rename <- reg_055_pred %>%
  rename(reg_055_pred_class = .pred_class) # .pred_class is the default variable name output from predict()

knn_055_pred_rename <- reg_055_pred %>%
  rename(knn_055_pred_class = .pred_class)

rf_055_pred_rename <- reg_055_pred %>%
  rename(rf_055_pred_class = .pred_class)

## remove the Text column (cannot export from TDM)
gbear_055_notext <- gbear_055 %>%
  select(-Text)

## combine into one dataframe
gbear_055_preds <- cbind(gbear_055_notext, reg_055_pred_rename, knn_055_pred_rename, rf_055_pred_rename)

## 0.6 gamma threshold
reg_06_pred_rename <- reg_06_pred %>%
  rename(reg_06_pred_class = .pred_class) # .pred_class is the default variable name output from predict()

knn_06_pred_rename <- reg_06_pred %>%
  rename(knn_06_pred_class = .pred_class)

rf_06_pred_rename <- reg_06_pred %>%
  rename(rf_06_pred_class = .pred_class)

## remove the Text column (cannot export from TDM)
gbear_06_notext <- gbear_06 %>%
  select(-Text)

## combine into one dataframe
gbear_06_preds <- cbind(gbear_06_notext, reg_06_pred_rename, knn_06_pred_rename, rf_06_pred_rename)

## 0.65 gamma threshold
reg_065_pred_rename <- reg_065_pred %>%
  rename(reg_065_pred_class = .pred_class) # .pred_class is the default variable name output from predict()

knn_065_pred_rename <- reg_065_pred %>%
  rename(knn_065_pred_class = .pred_class)

rf_065_pred_rename <- reg_065_pred %>%
  rename(rf_065_pred_class = .pred_class)

## remove the Text column (cannot export from TDM)
gbear_065_notext <- gbear_065 %>%
  select(-Text)

## combine into one dataframe
gbear_065_preds <- cbind(gbear_065_notext, reg_065_pred_rename, knn_065_pred_rename, rf_065_pred_rename)

## write the csv
write_csv(gbear_05_preds, 
          here("predictions", "grizzly_bear_05_preds_gamma.csv"))

write_csv(gbear_055_preds, 
          here("predictions", "grizzly_bear_055_preds_gamma.csv"))

write_csv(gbear_06_preds, 
          here("predictions", "grizzly_bear_06_preds_gamma.csv"))

write_csv(gbear_065_preds, 
          here("predictions", "grizzly_bear_065_preds_gamma.csv"))

# 5. Export the csv from TDM
#----

## Define the path to the file to be exported (will have to update for each of the four files)
data_to_export <- "/home/ec2-user/SageMaker/classification/predictions/grizzly_bear_05_preds_gamma.csv"
#data_to_export <- "/home/ec2-user/SageMaker/classification/predictions/grizzly_bear_055_preds_gamma.csv"
#data_to_export <- "/home/ec2-user/SageMaker/classification/predictions/grizzly_bear_06_preds_gamma.csv"
#data_to_export <- "/home/ec2-user/SageMaker/classification/predictions/grizzly_bear_065_preds_gamma.csv"

## Define the S3 bucket path
s3_path <- "s3://pq-tdm-studio-results/tdm-ale-data/a1290/results"

## Construct the AWS CLI command
aws_command <- paste("aws s3 cp" ,data_to_export, s3_path)

## Execute the CLI command
system(aws_command)











