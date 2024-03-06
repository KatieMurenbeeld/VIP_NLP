library(stringr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(janeaustenr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()


# read in the data
article_codes <- read.csv(file = "data/original/new_article_coding.csv")
original_articles <- read_csv(here::here("data/processed/02_module_articles_2024-03-06.csv"))

articles_text <- original_articles %>%
  group_by(doc_id) %>%
  mutate(title = str_extract(text,
                            regex("(?<=Title:).*")),
         full_text = str_extract(text, 
                                 regex("(?<=Full text:).[\\s\\S]*"))) %>%
  fill(title, .direction = "downup") %>%
  drop_na(full_text) %>%
  select(-text) %>%
  ungroup() 

articles_text <- articles_text[!duplicated(articles_text$title), ]
articles_text$title <- tolower(trimws(articles_text$title))

article_codes$Title <- tolower(trimws(article_codes$Title))

code_titles <- unique(article_codes$Title)
tdm_titles <- trimws(unique(articles_text$title))

# Join the articles text to the coded data
articles_joined <- left_join(articles_text, article_codes, by = c("title" = "Title"))

tidy_articles <- articles_text %>%
  unnest_tokens(word, full_text)

tidy_articles %>%
  count(word, sort = TRUE) 

data("stop_words")

tidy_articles %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

tidy_articles %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
