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
  select(Link, Article_Text)

# tokenize for sentences
# remove punctuation 
sentences <- articles_filter %>%
  unnest_tokens(sentence, Article_Text, token = "sentences")
sentences <- gsub('[[:punct:][:digit:] ]+',' ',sentences$sentence)

# Create two word embedding models
# test accuracy by seeing how well the models will predict x
# a:b::y:x
# a - b + y = x

set.seed(2486)
# for each model type and either with hs or ns test dim = c(50, 100, 200, 300, 400, 500)
# window = c(5, 10, 15)
model_sg_hs <- word2vec(sentences, type = "skip-gram", 
                        dim = 50, window = 5,
                        hs = TRUE, iter = 5)
model_cb_hs <- word2vec(sentences, type = "cbow", 
                        dim = 50, window = 5,
                        hs = TRUE, iter = 5)

model_sg_hs_100 <- word2vec(sentences, type = "skip-gram", 
                        dim = 100, window = 5,
                        hs = TRUE, iter = 5)
model_cb_hs_100 <- word2vec(sentences, type = "cbow", 
                        dim = 100, window = 5,
                        hs = TRUE, iter = 5)

sg.p <- predict(model_sg_hs, newdata = c("grizzly", "bear", "grey"), type = "embedding")
sg.p <- sg.p["grizzly", ] - sg.p["bear", ] + sg.p["grey", ]
predict(model_sg_hs, newdata = sg.p, type = "nearest", top_n = 5)

cb.p <- predict(model_cb_hs, newdata = c("grizzly", "bear", "feral"), type = "embedding")
cb.p <- cb.p["grizzly", ] - cb.p["bear", ] + cb.p["feral", ]
predict(model_cb_hs, newdata = cb.p, type = "nearest", top_n = 5)

sg.p100 <- predict(model_sg_hs_100, newdata = c("grizzly", "bear", "feral"), type = "embedding")
sg.p100 <- sg.p100["grizzly", ] - sg.p100["bear", ] + sg.p100["feral", ]
predict(model_sg_hs_100, newdata = sg.p100, type = "nearest", top_n = 5)

cb.p100 <- predict(model_cb_hs_100, newdata = c("grizzly", "bear", "feral"), type = "embedding")
cb.p100 <- cb.p100["grizzly", ] - cb.p100["bear", ] + cb.p100["feral", ]
predict(model_cb_hs_100, newdata = cb.p100, type = "nearest", top_n = 10)

# for negative sampling also test negative = c(5, 10, 15)
model_sg_ns <- word2vec(sentences, type = "skip-gram", dim = 50, iter = 5)
model_cb_ns <- word2vec(sentences, type = "cbow", dim = 50, iter = 5)

types <- c("skip-gram", "cbow")
#dimens <- c(50, 100, 200, 300, 400, 500)
dimens <- c(50, 100)
#windows <- c(5, 10, 15, 20, 25, 30)
windows <- c(5, 10)
#n_samples <- c(5, 10, 15, 20, 25, 30)
n_samples <- c(5, 10)
               
wv_df <- data.frame(
  term = character(),
  similarity = numeric(),
  rank = numeric(),
  type = character(),
  dim = numeric(),
  window = numeric()
)

for (t in types) {
  for (d in dimens){
    for(w in windows){
      model <- word2vec(sentences, type = t, 
                      dim = d, window = w,
                      hs = TRUE, iter = 5)
      pred <- predict(model, newdata = c("grizzly", "bear", "feral"), type = "embedding")
      pred <- pred["grizzly", ] - pred["bear", ] + pred["feral", ]
      out <- predict(model, newdata = pred, type = "nearest", top_n = 10)
      out$type <- t
      out$dim <- d
      out$window <- w
      wv_df <- rbind(wv_df, out)
    }
  }
}



