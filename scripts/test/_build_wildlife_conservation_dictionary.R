library(word2vec)
library(tidyverse)
library(tidytext)
library(tm)
library(slider)
library(purrr)
library(widyr)
library(furrr)
library(ggplot2)


# Load the cleaned data
articles_text_clean <- read_csv(here::here("data/processed/clean_text_2024-09-16.csv"))

# Load in corpus of wildlife conservation pdfs


# Load in articles not related to wildlife conservation 
# (see Ulibarri I think they compared things to word2vec or wikipedia articles?)

# I think I want the tf-idf for words?

tidy_text <-  articles_text_clean %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word)) 

dtm <- tidy_text %>%
  count(Title.x, word, sort = TRUE) %>%
  bind_tf_idf(word, Title.x, n) %>%
  cast_dtm(Title.x, word, tf_idf)

article_tf_idf <- tidy_text %>%
  count(Title.x, word, sort = TRUE) %>%
  bind_tf_idf(word, Title.x, n)

article_tf_idf %>%
  arrange(desc(tf_idf))

