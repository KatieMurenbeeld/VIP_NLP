library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)


#---Load the data----
gbear_05_preds <- read_csv(here::here("output/predictions/grizzly_bear_05_preds_gamma.csv"))
gbear_055_preds <- read_csv(here::here("output/predictions/grizzly_bear_055_preds_gamma.csv"))
gbear_06_preds <- read_csv(here::here("output/predictions/grizzly_bear_06_preds_gamma.csv"))
gbear_065_preds <- read_csv(here::here("output/predictions/grizzly_bear_065_preds_gamma.csv"))

training <- read_csv(here::here("data/processed/clean_text_2024-09-16.csv"))

# for the training data drop any rows with NA for Value_Orientation
training <- training[!is.na(training$Value_Orientation), ]
train_df <- training %>%
  mutate(type = "training") %>%
  dplyr::select(type, Value_Orientation) %>%
  rename("value" = "Value_Orientation")

# what could I look at?
# compare histograms of the different models and thresholds 
## (also compare to the training data)
# any relationship between gamma and the classification
# any temporal trends in the predicted classification 

hist(gbear_05_preds$reg_05_pred_class)
hist(gbear_05_preds$knn_05_pred_class)
hist(gbear_05_preds$rf_05_pred_class)
hist(train_df$Value_Orientation)

t05_reg <- data.frame(
  type = "regression",
  value = gbear_05_preds$reg_05_pred_class)
t05_knn <- data.frame(
  type = "knn", 
  value = gbear_05_preds$knn_05_pred_class
)
t05_rf <- data.frame(
  type = "random forest", 
  value = gbear_05_preds$rf_05_pred_class
)

t05_all <- rbind(t05_reg, t05_knn, t05_rf, train_df)
t05_all <- t05_all %>%
  mutate(gamma_thres = "gamma thres = 0.5")

t055_reg <- data.frame(
  type = "regression",
  value = gbear_055_preds$reg_055_pred_class)
t055_knn <- data.frame(
  type = "knn", 
  value = gbear_055_preds$knn_055_pred_class
)
t055_rf <- data.frame(
  type = "random forest", 
  value = gbear_055_preds$rf_055_pred_class
)

t055_all <- rbind(t055_reg, t055_knn, t055_rf, train_df)
t055_all <- t055_all %>%
  mutate(gamma_thres = "gamma thres = 0.55")

t06_reg <- data.frame(
  type = "regression",
  value = gbear_06_preds$reg_06_pred_class)
t06_knn <- data.frame(
  type = "knn", 
  value = gbear_06_preds$knn_06_pred_class
)
t06_rf <- data.frame(
  type = "random forest", 
  value = gbear_06_preds$rf_06_pred_class
)

t06_all <- rbind(t06_reg, t06_knn, t06_rf, train_df)
t06_all <- t06_all %>%
  mutate(gamma_thres = "gamma thres = 0.6")

t065_reg <- data.frame(
  type = "regression",
  value = gbear_065_preds$reg_065_pred_class)
t065_knn <- data.frame(
  type = "knn", 
  value = gbear_065_preds$knn_065_pred_class
)
t065_rf <- data.frame(
  type = "random forest", 
  value = gbear_065_preds$rf_065_pred_class
)

t065_all <- rbind(t065_reg, t065_knn, t065_rf, train_df)
t065_all <- t065_all %>%
  mutate(gamma_thres = "gamma thres = 0.65")


data_all <- rbind(t05_all, t055_all, t06_all, t065_all)

t05_hist <- t05_all %>%
  ggplot(aes(x = value, fill = type)) +
  geom_histogram(color = "#e9ecef", alpha = 0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#678b43", "red4")) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  theme_bw() +
  facet_wrap(~type, ncol = 4) + 
  labs(fill="")
t05_hist

test_hist <- data_all %>%
  ggplot(aes(x = value, fill = type)) +
  geom_histogram(color = "#e9ecef", alpha = 0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#678b43", "red4")) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  theme_bw() +
  #theme(axis.title.x = "Predicted Value Orientation") +
  facet_grid(gamma_thres ~ type) + 
  labs(fill="")
test_hist
ggsave(here::here(paste0("output/plots/test_gamma_thres_pred_value_orientation_", 
                         Sys.Date(), ".png")), 
       height = 12, width = 12, dpi = 300)

# Build dataset with different distributions
data <- data.frame(
  type = c( rep("variable 1", 1000), rep("variable 2", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=4) )
)

# Represent it
p <- data %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
#  theme_ipsum() +
  labs(fill="")
p



