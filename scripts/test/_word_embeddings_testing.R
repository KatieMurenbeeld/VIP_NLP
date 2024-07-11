library(word2vec)
library(tidyverse)
library(tidytext)


library(udpipe)
data(brussels_reviews, package = "udpipe")
x <- subset(brussels_reviews, language == "nl")
x <- tolower(x$feedback)
model_nl <- word2vec(x = x, dim = 15, iter = 20)
emb_nl   <- as.matrix(model_nl)
head(emb_nl)
emb_nl   <- predict(model_nl, c("bus", "toilet", "unknownword"), type = "embedding")
emb_nl
nn_nl    <- predict(model_nl, c("bus", "toilet"), type = "nearest", top_n = 5)
nn_nl

# load the text data

articles <- read.csv(here::here("data/processed/article_text_2024-07-11.csv"))

# remove/filter for titles with Please try again.
# select article text
# make all lower case and remove punctuation 

articles_filter <- articles %>%
  filter(str_detect(Title, "Please try again.", negate = TRUE)) %>%
  select(Link, Article_Text)



text <- tolower(articles_filter$Article_Text)

sentences <- articles_filter %>%
  unnest_tokens(sentence, Article_Text, token = "sentences")

sentences <- gsub('[[:punct:][:digit:] ]+',' ',sentences$sentence)

set.seed(2486)
model_sg <- word2vec(sentences, type = "skip-gram", dim = 15, iter = 20, hs = FALSE)
emb_sg <- as.matrix(model_sg)

nn_sg <- predict(model_sg, c("dam", "pig", "montana"), type = "nearest", top_n = 5)
nn_sg

model_cb <- word2vec(sentences, type = "cbow", dim = 15, iter = 20)
emb_cb <- as.matrix(model_cb)

nn_cb <- predict(model_cb, c("dam", "pig", "montana"), type = "nearest", top_n = 5)
nn_cb

vocab_sg <- summary(model_sg, type = "vocabulary")
vocab_cb <- summary(model_cb, type = "vocabulary")

vector_sg <- emb_sg["pig", ] - emb_sg["texas", ]
predict(model_sg, vector_sg, type = "nearest", top_n = 10)

