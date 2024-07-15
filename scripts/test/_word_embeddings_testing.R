library(word2vec)
library(tidyverse)
library(tidytext)
library(slider)
library(purrr)
library(widyr)
library(furrr)
library(ggplot2)

#---build my own word embeddings following along with Hvitfeldt and Silge 2022 ----

# Load the wildlife conflict articles

articles <- read.csv(here::here("data/processed/article_text_2024-07-11.csv"))

# remove/filter for titles with "Please try again." --> article not on newsbank anymore
articles_filter <- articles %>%
  filter(str_detect(Title, "Please try again.", negate = TRUE)) %>%
  select(Link, Article_Text)

# create a tidy text data frame
tidy_text <- articles_filter %>% 
  unnest_tokens(word, Article_Text) %>%
  add_count(word) %>%
  filter(n >= 50) %>%
  select(-n)

# create a nested data frame with one row per article
nested_words <- tidy_text %>%
  nest(words = c(word))

# create a function to identify the skipgram window to calculate skipgram probabilities

slide_windows <- function(tbl, window_size){
  skipgrams <- slider::slide(
    tbl,
    ~.x,
    .after = window_size - 1,
    .step = 1,
    .complete = TRUE
  )
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))
  
  out %>%
    transpose() %>%
    pluck("result") %>%
    compact() %>%
    bind_rows()
}

# calculate the point-wise mutual information (PMI) using parallel processing
## "the logarithm of the probability of finding two words together, normalized for the probability of finding each of the words alone."
## pg. 79
## higher PMI means the two words are likely to occur together

plan(multisession())

tidy_pmi <- nested_words %>%
  mutate(words = future_map(words, slide_windows, 4L)) %>%
  unnest(words) %>%
  unite(window_id, Link, window_id) %>%
  pairwise_pmi(word, window_id)

# "determine the word vectors for the PMI values using singular value decomposition (SVD)" pg. 80
tidy_word_vector <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 200 # increased maxit from 100 to 200, with 100 received a non-convergence warning
  )


# create a function that finds the nearest word(s) to any given example word in the "new feature space"
nearest_neighbors <- function(df, token){
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    ) (item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vector %>%
  nearest_neighbors("grizzly")

tidy_word_vector %>%
  nearest_neighbors("bear")

tidy_word_vector %>%
  nearest_neighbors("pig")

tidy_word_vector %>%
  nearest_neighbors("beaver")

tidy_word_vector %>%
  nearest_neighbors("wild")

tidy_word_vector %>%
  nearest_neighbors("agriculture")

tidy_word_vector %>%
  nearest_neighbors("population")

# investigate what principal components explain the most variation in the articles.

tidy_word_vector %>%
  filter(dimension <= 24) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup() %>%
  mutate(item1 = reorder_within(item1, value, dimension)) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    x = NULL,
    y = "value",
    title = "First 24 principal components for text of wildlife articles",
    subtitle = "Top words contributing to the components that explain",
    "the most variation"
  )

tidy_word_vector %>%
  filter(dimension %in% (25:48) ) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup() %>%
  mutate(item1 = reorder_within(item1, value, dimension)) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    x = NULL,
    y = "value",
    title = "Principal components 25-48 for text of wildlife articles",
    subtitle = "Top words contributing to the components that explain",
    "the most variation"
  )

# set up for use in a model "treat each document as a collection of wards and summarize the word 
# embeddings into document embeddings" pg. 87
word_matrix <- tidy_text %>%
  count(Link, word) %>%
  cast_sparse(Link, word, n)

embedding_matirx <- tidy_word_vector %>%
  cast_sparse(item1, dimension, value)

doc_matrix <- word_matrix %*% embedding_matirx

dim(doc_matrix)

#----Playing with word2vec----
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

nn_sg <- predict(model_sg, c("grizzly", "bear", "pig", "beaver", "wild"), type = "nearest", top_n = 5)
nn_sg

model_cb <- word2vec(sentences, type = "cbow", dim = 15, iter = 20)
emb_cb <- as.matrix(model_cb)

nn_cb <- predict(model_cb, c("dam", "pig", "montana"), type = "nearest", top_n = 5)
nn_cb

vocab_sg <- summary(model_sg, type = "vocabulary")
vocab_cb <- summary(model_cb, type = "vocabulary")

vector_sg <- emb_sg["pig", ] - emb_sg["texas", ] + emb_sg["beaver", ]
predict(model_sg, vector_sg, type = "nearest", top_n = 10)

