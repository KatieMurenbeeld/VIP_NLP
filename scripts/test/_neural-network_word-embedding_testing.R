library(tidyverse)
library(tidytext)
library(tidymodels)
library(textrecipes)
library(keras3)
keras3::install_keras(backend = "tensorflow")
library(ggplot2)


## Pseudocode:
### 1. Load the data
### 2. Create a dataframe with the text and a binary value orientation
### 3. Split the data into a training and testing set
### 4. Determine the number of words per article
### 5. Using recipes(?) tokenize, tokenfilter(max_words = 20,000) <-- sets vocab
### 5. sequence_onehot(sequence_length = max_length) <-- based on # words in art
### 6. Tidy the text_recipe with number = 3 (for the sequence_onehot)
### 7. Load the word embeddings from a previous script
### 8. Combine the training tokens with the word embedding vector
### 9. Create the model
### 10. Set the optimizer, loss and metrics
### 11. Fit the model
### 12. Visualize training and validation loss and metrics
### 13. Visualize a confusion matrix

# 1. Load the wildlife conflict articles: the text with the code

articles <- read.csv(here::here("data/processed/article_text_2024-07-11.csv"))
text_code <- read.csv(here::here("data/processed/article_text_codes_2024-07-11.csv"))

# remove/filter for titles with "Please try again." --> article not on newsbank anymore
articles_filter <- text_code %>%
  filter(str_detect(Title.x, "Please try again.", negate = TRUE)) %>%
  select(Link, Article_Text, Value_Orientation) 

# 2. Create a dataframe with the unique article links and the article text
article_text <- articles_filter[!duplicated(articles_filter$Link),] %>%
  select(Link, Article_Text)

# Create a binary for the value orientation from the mean value for each article
binary_vo <- articles_filter %>%
  group_by(Link) %>%
  summarise(Value_Orientation = mean(as.numeric(Value_Orientation))) %>%
  within(.,{
    value_simple=NA
    value_simple[Value_Orientation <= 3.5] = 0
    value_simple[Value_Orientation > 3.5] = 1
  }) 

# Join the binary value orientation to the article text df
article_text_vo <- left_join(article_text, binary_vo, by = "Link") %>%
  drop_na()

# 3. Split the data into training and testing
wildlife_split <- article_text_vo %>%
  initial_split()
  
wildlife_train <- training(wildlife_split)
wildlife_test <- testing(wildlife_split)

# 4. Determine the number of words per article
wildlife_train %>%
  mutate(n_words = tokenizers::count_words(Article_Text)) %>%
  ggplot(aes(n_words)) +
  geom_bar() +
  labs(x = "Number of words per article",
       y = "Number of articles")

# Number of words per article could be capped at 2000

# 5. Preprocess articles for neural networks with textrecipes

max_words <- 2e4
max_length <- 2e3

wild_rec <- recipe(~ Article_Text, data = wildlife_train) %>%
  step_tokenize(Article_Text) %>%
  step_tokenfilter(Article_Text, max_tokens = max_words) %>%
  step_sequence_onehot(Article_Text, sequence_length = max_length)

wild_prep <- prep(wild_rec)
wild_train <- bake(wild_prep, new_data = NULL, composition = "matrix")
dim(wild_train)

# 6. Bring in word embeddings created in a previous script
wild_pwe <- readRDS(here::here("data/processed/wildlife_articles_pwe_2024-07-22.RDS"))
wild_pwe_matrix <- readRDS(here::here("data/processed/wildlife_articles_pwe_matrix_2024-07-22.RDS"))

## Make wild_pwe_matrix into a data frame with column "token" made from rownames
wild_pwe_matrix <- as.matrix(wild_pwe_matrix)
wild_pwe_df <- as.data.frame(wild_pwe_matrix)
wild_pwe_df <- wild_pwe_df[,1:12]
wild_pwe_df$token <- rownames(wild_pwe_df)

tidy(wild_prep, number = 3)

pwe_matrix <- tidy(wild_prep, 3) %>%
  select(token) %>%
  left_join(wild_pwe_df, by = "token") %>%
  mutate_all(replace_na, 0) %>%
  dplyr::select(-token) %>%
  as.matrix() %>%
  rbind(0, .)

# Got out of order

# 9. Create dense neural network model (dnn) with pretrained embeddings

dense_model_pte <- keras3::keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, 
                  output_dim = ncol(pwe_matrix),
                  input_length = max_length) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

# Keep getting an incompatible architecture error (have 'arm64', need 'x86_64')

