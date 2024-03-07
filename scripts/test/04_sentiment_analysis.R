library(stringr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(ggplot2)
library(textdata)


# read in the data
joined_articles <- read.csv(file = here::here("data/processed/03_joined-article_code_text.csv"))

# tidy the data

## load stop words
data("stop_words")
my_stop_words <- data.frame(c("facebook", "twitter", "whatsapp", "print", "sms", "copy", "email", "http", "https")) 
colnames(my_stop_words) <-("word")

tidy_articles <- joined_articles %>%
  unnest_tokens(word, full_text) %>%
  anti_join(stop_words) %>%
  anti_join(my_stop_words) %>%
  mutate(stem = wordStem(word))

# create a data frame with the bing sentiment lexicon

bing_sent <- tidy_articles %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

afinn_sent <- tidy_articles %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  ungroup()

nrc_sent <- tidy_articles %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Plot the sentiments

# compare the various sentiments to the value orientation





