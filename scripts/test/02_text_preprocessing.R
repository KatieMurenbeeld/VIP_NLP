library(stringr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(ggplot2)
library(tigris)

# Read in the data
text_codes <- read.csv(here::here("data/processed/article_text_codes_2024-04-04.csv"))

# Preprocess the data
articles_text <- text_codes %>%
  filter(Species == "Grizzly Bear" | Species == "Grizzly Bears")

articles_spread <- articles_text %>%
  pivot_wider( values_from = Conflict_Type)

# Drop duplicated 
#articles_text <- articles_text[!duplicated(articles_text$Title.x), ]


## Make all the article titles lowercase
articles_text$title <- tolower(trimws(articles_text$title))
article_codes$Title <- tolower(trimws(article_codes$Title))

## Tokenize
tidy_articles <- articles_joined %>%
  unnest_tokens(word, full_text)

## Check the most common words
tidy_articles %>%
  count(word, sort = TRUE) 








