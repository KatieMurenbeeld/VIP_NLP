# Load the required libraries
library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(stopwords)

library(recipes)
library(modeldata)

library(ggplot2)

data(tate_text)
tate_rec <- recipe(~., data = tate_text) %>%
  step_tokenize(medium) %>%
  step_stopwords(medium)

tate_obj <- tate_rec %>%
  prep()

bake(tate_obj, new_data = NULL, medium) %>%
  slice(1:2)

bake(tate_obj, new_data = NULL) %>%
  slice(2) %>%
  pull(medium)

tidy(tate_rec, number = 2)
tidy(tate_obj, number = 2)


################
# Load the cleaned data
articles_text_clean <- read_csv(here::here("data/processed/clean_text_2025-05-13.csv"))

# drop any rows with NA for Value_Orientation
articles_text_clean <- articles_text_clean[!is.na(articles_text_clean$Value_Orientation), ]

# Load the unlabeled documents to make sure predict.model_fit() will work
# Note this is a very small dataset of 6 wildlife articles without a value orientation
unlabeled_articles <- read_csv(here::here("data/processed/newdata_for_model_testing_text_2024-11-06.csv"))
unlabeled_articles <- unlabeled_articles %>%
  dplyr::select(Article_Text)

# select relevant variables from articles_text_clean
text2wvo <- articles_text_clean %>%
  dplyr::select(Article_Text, Value_Orientation)

## make Value_Orientation a factor
text2wvo$Value_Orientation <- as.factor(text2wvo$Value_Orientation)

head(articles_text_clean)


hist(as.numeric(articles_text_clean$Value_Orientation))

articles_text_clean %>%
  ggplot( aes(x=Value_Orientation, color=Focus, fill=Focus)) +
  geom_histogram(alpha=0.6, binwidth = 1) +
  #scale_fill_viridis(discrete=TRUE) +
  #scale_color_viridis(discrete=TRUE) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Value Orientation") +
  facet_wrap(~Focus)

articles_text_clean %>%
  ggplot( aes(x=Value_Orientation, color=Species, fill=Species)) +
  geom_histogram(alpha=0.6, binwidth = 1) +
  #scale_fill_viridis(discrete=TRUE) +
  #scale_color_viridis(discrete=TRUE) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Value Orientation") +
  facet_wrap(~Species)

articles_text_clean %>%
  ggplot( aes(x=Value_Orientation, color=Publication_State, fill=Publication_State)) +
  geom_histogram(alpha=0.6, binwidth = 1) +
  #scale_fill_viridis(discrete=TRUE) +
  #scale_color_viridis(discrete=TRUE) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Value Orientation") +
  facet_wrap(~Publication_State)

articles_text_clean %>%
  mutate(year = year(Published_Date)) %>%
  group_by(year, Focus) %>%
  summarise(ave_vo = mean(Value_Orientation)) %>%
  ggplot(aes(x = year, y = ave_vo, color = Focus, fill = Focus)) +
  geom_line() + 
  theme(
    #legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Value Orientation")

articles_text_clean %>%
  mutate(year = year(Published_Date)) %>%
  group_by(year, Focus) %>%
  summarise(ave_vo = mean(Value_Orientation)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = ave_vo)) +
  geom_line() + 
  theme(
    #legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Value Orientation") + 
  facet_wrap(~Focus)

articles_text_clean %>%
  mutate(year = year(Published_Date)) %>%
  group_by(year, Species) %>%
  summarise(ave_vo = mean(Value_Orientation)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = ave_vo)) +
  geom_line() + 
  theme(
    #legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Value Orientation") + 
  facet_wrap(~Species)

# then split the data into training and testing sets
text_split <- initial_split(text2wvo, strat = Value_Orientation)

text_train <- training(text_split)
text_test <- testing(text_split)

# create the text training recipe (model)
wildlife_stop_words <- c("p", "br", "strong", "targetednews.com",
                                    "grizzly", "grizzlies", "bears", "bear", 
                                    "wolf", "wolves", "coyote", "coyotes", 
                                    "pigs", "pig", "beaver", "beavers",
                                    "bison", "mountain lion", "mountain lions",
                                    "bobcat", "bobcats", "alligator", "alligators",
                                    "lynx", "wolverine", "wolverines", "monarch", 
                                    "butterfly", "butterflies", "turtle", "turtles",
                                    "racoon", "racoons", "elk", "elks", 
                                    "pronghorn", "pronghorns", "prairie dog", 
                                    "prairie dogs", "mink", "porcupine", 
                                    "porcupines", "amp", "div", "class", "span", 
                                    "href", "wildlife", "wild", "fish", "animal",
                                    "animals", "species")


text_rec <- 
  recipe(Value_Orientation ~ Article_Text, data = text_train)

text_rec_v1 <- text_rec %>%
  step_tokenize(Article_Text) %>%
  step_stopwords(Article_Text, stopword_source = "snowball") %>%
  step_stopwords(Article_Text, custom_stopword_source = wildlife_stop_words) %>%
  #step_tokenfilter(Article_Text, max_tokens = tune(), min_times = 100) %>%
  step_tokenfilter(Article_Text, max_tokens = 1000, min_times = 100) %>%
  step_tfidf(Article_Text)


text_obj <- prep(text_rec_v1)
baked_data <- bake(text_obj, text_train)

##### test random forest

rf_spec <- rand_forest(mtry = 20, 
                       trees = 100,
                       min_n = 4
) %>%
  #set_engine("ranger") %>%
  set_engine("ranger", probability = TRUE) %>% # set probability = TRUE
  set_mode("classification")

## create workflow with tuneable model
rf_wf <- workflow() %>%
  add_recipe(text_rec_v1) %>%
  add_model(rf_spec)
rf_wf

# From the training data split, create the cross validation resampling folds for training/tuning
set.seed(234)
text_folds <- vfold_cv(text_train)

# For resamples, save the predictions
cntrl <- control_resamples(save_pred = TRUE)

rf_final_rs <- fit_resamples(
  rf_wf, 
  text_folds, 
  control = control_resamples(save_pred = TRUE)
)

rf_final_fitted <- last_fit(rf_wf, text_split)
collect_metrics(rf_final_fitted)

collect_predictions(rf_final_fitted) %>%
  conf_mat(truth = Value_Orientation, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

rf_final_fitted$.predictions

rf_final_fitted$.metrics

rf_final_fitted$.split

