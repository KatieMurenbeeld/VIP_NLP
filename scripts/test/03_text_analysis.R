library(stringr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(ggplot2)

# read in the data
article_codes <- read.csv(file = "data/original/new_article_coding.csv")
original_articles <- read_csv(here::here("data/processed/02_module_articles_2024-03-06.csv"))

# Preprocess the data
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

## Get rid of duplicate
articles_text <- articles_text[!duplicated(articles_text$title), ]

## Make all the article titles lowercase
articles_text$title <- tolower(trimws(articles_text$title))
article_codes$Title <- tolower(trimws(article_codes$Title))

## Join the articles text to the coded data
articles_joined <- left_join(articles_text, article_codes, by = c("title" = "Title"))

## Write to csv
write.csv(articles_joined, here::here("data/processed/03_joined-article_code_text.csv"), row.names = FALSE)

## Tokenize
tidy_articles <- articles_joined %>%
  unnest_tokens(word, full_text)

## Check the most common words
tidy_articles %>%
  count(word, sort = TRUE) 

## There are many words that won't add much to the analysis
## These are called stop words
## Load the built in stop words list
data("stop_words")

## Create a small data frame of your own stop words for this project 
## This needs to be a data frame so that you can use "anti-join()"
## You can add your own words to this list
my_stop_words <- data.frame(c("facebook", "twitter", "whatsapp", "print", "sms", "copy", "email", "http", "https")) 
colnames(my_stop_words) <-("word")

## Remove stop words
tidy_articles <- tidy_articles %>%
  anti_join(stop_words) %>%
  anti_join(my_stop_words)

## "Stem" the words
tidy_articles <- tidy_articles %>% 
  mutate(stem = wordStem(word))

## Check the new word count
tidy_articles %>%
  count(word, sort = TRUE) 

## Check the stemmed word count
tidy_articles %>%
  count(stem, sort = TRUE) 

## Plot the most common words in the tidy text data frame

tidy_articles %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

## Can also plot by species

unique(tidy_articles$Species) # Need to clean up names

### Can enter your species of interest here
species <- c("Grizzly Bears", "Grizzly Bear")

tidy_articles %>%
  filter(Species %in% species) %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=10) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle(paste(species, "10 most common words"))

tidy_articles %>%
  filter(Species %in% species) %>%
  count(stem, sort = TRUE) %>%
  slice_max(n, n=10) %>%
  filter(n > 5) %>%
  mutate(stem = reorder(stem, n)) %>%
  ggplot(aes(n, stem)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle(paste(species, "10 most common stems"))

## Could also plot by Focus or any other variable

unique(tidy_articles$Focus) # Need to clean up names

focus <- c("Wildlife", "Wildllife")

tidy_articles %>%
  filter(Focus %in% focus) %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=10) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle(paste("Focus is ", focus, "10 most common words"))

tidy_articles %>%
  filter(Focus %in% focus) %>%
  count(stem, sort = TRUE) %>%
  slice_max(n, n=10) %>%
  filter(n > 5) %>%
  mutate(stem = reorder(stem, n)) %>%
  ggplot(aes(n, stem)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle(paste("Focus is ", focus, "10 most common stems"))


## N-grams

df_bigrams <- articles_joined %>%
  unnest_tokens(bigram, full_text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))


# In order to remove the stop words we have to first separate out the two words into word1 and word2
bigrams_separated <- df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Then filter out stopwords and our own stop words for word1 and word2
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word)%>% 
  mutate(stem1 = wordStem(word1)) %>% # not sure if we want to stem the words for bigrams or not, but would do so here.
  mutate(stem2 = wordStem(word2))

# Then, calculate new bigram counts. This is optional, but good to check. 
bigram_counts <- bigrams_filtered %>% 
  #count(word1, word2, sort = TRUE)
  count(stem1, stem2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  #  unite(bigram, word1, word2, sep = " ") %>% 
  unite(bigram, stem1, stem2, sep = " ")


bigrams_united %>% 
  filter(Species %in% species) %>% 
  count(bigram, sort=TRUE) %>%
  slice_max(n, n=20) %>%  # set the number of words to show
  filter(n > 1) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(n, bigram)) +
  geom_col(fill = "blue") +
  labs(title = paste("20 Most bigrams in Articles About", species), # update the Species
       y = NULL)

