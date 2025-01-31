library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(MetBrewer)


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
hist(train_df$value)

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



# predicted classification through time
gbear_05_time <- gbear_05_preds %>%
  dplyr::select(Date, reg_05_pred_class, knn_05_pred_class, rf_05_pred_class) %>%
  mutate(gamma_thres = "gamma thres = 0.5")

gbear_055_time <- gbear_055_preds %>%
  dplyr::select(Date, reg_055_pred_class, knn_055_pred_class, rf_055_pred_class) %>%
  mutate(gamma_thres = "gamma thres = 0.55")

gbear_06_time <- gbear_06_preds %>%
  dplyr::select(Date, reg_06_pred_class, knn_06_pred_class, rf_06_pred_class) %>%
  mutate(gamma_thres = "gamma thres = 0.6")

gbear_065_time <- gbear_065_preds %>%
  dplyr::select(Date, reg_065_pred_class, knn_065_pred_class, rf_065_pred_class) %>%
  mutate(gamma_thres = "gamma thres = 0.65")

gbear_05_time$Date <- as.Date(gbear_05_time$Date) 

gbear_05_time <- gbear_05_time %>%
  mutate(year = year(Date), 
         month = month(Date))

gbear_05_time %>% 
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count)) + 
  geom_line() 

gbear_05_time %>% 
  group_by(year, reg_05_pred_class) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count)) + 
  geom_line(aes(color = as.factor(reg_05_pred_class)))+ 
  scale_color_met_d("Derain")

vlines <- tibble(word = c("End of Grizzly Hunting", "USFWS Updates Recovery Plan", "Bitterroot Reinto Plan",
                   "Yellowstone Delisting", "Relisted", "Protection Stays", 
                   "MT ranch conflicts", "Wash Reintroduction", "Yellowstone Delist 2.0"), 
                 years = c(1991, 1993, 2000, 
                   2007, 2009, 2011,
                   2013, 2015, 2017))
gbear_05_time %>% 
  group_by(year, knn_05_pred_class) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count, fill = as.factor(knn_05_pred_class))) + 
  geom_area() + 
  geom_vline(data = vlines, aes(xintercept = years, group = word)) + 
  scale_fill_met_d("Derain") + 
  labs(title = "Number of Grizzly Bear Articles", 
       subtitle = "KNN Model\n(gamma threshold = 0.5)",
       x = "Year", y = "Count", 
       fill = "Predicted\nValue Orientation")
ggsave(here::here(paste0("output/plots/gbear_knn_mod_gt05_year_", 
                         Sys.Date(), ".png")),
       height = 10, width = 12, dpi = 300)

# what about "confusion matrices" between the different models?

reg_pred_counts <- t05_reg %>%
  group_by(as.factor(value)) %>%
  summarise(count_reg = n())

knn_pred_counts <- t05_knn %>%
  group_by(as.factor(value)) %>%
  summarise(count_knn = n())

rf_pred_counts <- t05_rf %>%
  group_by(as.factor(value)) %>%
  summarise(count_rf = n())

test <- left_join(reg_pred_counts, knn_pred_counts)

reg_value <- as.factor(t05_reg$value)
knn_value <- as.factor(t05_knn$value)
rf_value <- as.factor(t05_rf$value)

library(caret)
#Creating confusion matrix
reg_knn <- confusionMatrix(data=reg_value, reference = knn_value)
reg_knn_table <- reg_knn$table
reg_knn_df <- as.data.frame(reg_knn_table)
reg_knn_df <- reg_knn_df %>%
  rename(reg_preds = Prediction) %>%
  rename(knn_preds = Reference) %>%
  rename(counts = Freq)

plot <- ggplot(reg_knn_df)
plot + 
  geom_tile(aes(x=reg_preds, y=knn_preds, fill=counts)) +
  geom_text(aes(x = reg_preds, y = knn_preds, label = round(counts)), size = 2.5)+
  scale_x_discrete(name="Knn Predicted Class") + 
  scale_y_discrete(name="Reg. Predicted Class") +
  scale_fill_gradient(low = "white", high = "grey1")

rf_reg <- confusionMatrix(data=rf_value, reference = reg_value)
rf_reg_table <- rf_reg$table
rf_reg_df <- as.data.frame(rf_reg_table)
rf_reg_df <- rf_reg_df %>%
  rename(reg_preds = Reference) %>%
  rename(rf_preds = Prediction) %>%
  rename(counts = Freq)

plot <- ggplot(rf_reg_df)
plot + 
  geom_tile(aes(x = rf_preds, y = reg_preds, fill = counts)) +
  geom_text(aes(x = rf_preds, y = reg_preds, label = round(counts)), size = 2.5)+
  scale_x_discrete(name="Random Forest Predicted Class") + 
  scale_y_discrete(name="Reg. Predicted Class") +
  scale_fill_gradient(low = "white", high = "grey1")

rf_knn <- confusionMatrix(data=rf_value, reference = knn_value)
rf_knn_table <- rf_knn$table
rf_knn_df <- as.data.frame(rf_knn_table)
rf_knn_df <- rf_knn_df %>%
  rename(knn_preds = Reference) %>%
  rename(rf_preds = Prediction) %>%
  rename(counts = Freq)

plot <- ggplot(rf_knn_df)
plot + 
  geom_tile(aes(x = rf_preds, y = knn_preds, fill=counts)) +
  geom_text(aes(x = rf_preds, y = knn_preds, label = round(counts)), size = 2.5)+
  scale_x_discrete(name="Random Forest Predicted Class") + 
  scale_y_discrete(name="KNN Predicted Class") +
  scale_fill_gradient(low = "white", high = "grey1")

