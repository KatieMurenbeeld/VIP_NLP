library(sentimentr)
library(word2vec)
library(tidyverse)
library(tidytext)
library(slider)
library(purrr)
library(widyr)
library(furrr)
library(ggplot2)

# load the coded and clean articles
articles <- read.csv(here::here("data/processed/article_text_codes_2024-09-10.csv"))

# remove/filter for titles with Please try again.
# select article text
articles_filter <- articles %>%
  filter(str_detect(Title.x, "Please try again.", negate = TRUE)) %>%
  #filter(Species == "Beavers") %>%
  select(Link, Article_Text, Species)

articles_filter <- unique(articles_filter)

# tokenize for sentences
# remove punctuation 
sentences <- articles_filter %>%
  unnest_tokens(sentence, Article_Text, token = "sentences")
sentences <- gsub('[[:punct:][:digit:] ]+',' ',sentences$sentence)

#### tokenize for unigrams, remove stop words, and numbers
data("stop_words")
my_stop_words <- as.data.frame(c("a.m", "u.s", "state's"))
colnames(my_stop_words) <-("word")

all_words <- articles_filter %>%
  unnest_tokens(word, Article_Text) %>%
  filter(!grepl('[0-9]', word)) %>%
  filter(!grepl('[:punct:]', word)) %>%
  anti_join(my_stop_words, by = "word") %>%
  anti_join(stop_words, by = "word")  %>%
  count(word, sort = TRUE)

top_200_words <- head(all_words, n = 200)

#### Create word embedding model
set.seed(2486)
model_sg <- word2vec(sentences, type = "skip-gram", dim = 50, 
                     window = 30, hs = FALSE, 
                     negative = 5, iter = 5)


### create a test dictionary - predict words using the word2vec model from the top 200 words
test_dict_nn <- predict(model_sg, top_200_words$word, type = "nearest", top_n = 5)

test_dict_nn_df <- data.frame()
for (i in 1:200){
  #print(i)
  tmp_words <- as.data.frame(test_dict_nn[[i]]$term2)
  #print(tmp_words)
  test_dict_nn_df <- rbind(test_dict_nn_df, tmp_words)
}
colnames(test_dict_nn_df) <-("word")
test_dict_nn_df <- unique(test_dict_nn_df)


# basic sentimentr workflow
mytext <- get_sentences(sentences)
#sentiment(mytext)
jr_species_sent <- sentiment_by(get_sentences(sentences), list(Species))
jr_sent <- sentiment(mytext)

hist(jr_sent$sentiment)


