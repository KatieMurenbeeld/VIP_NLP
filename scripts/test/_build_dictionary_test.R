library(word2vec)
library(tidyverse)
library(tidytext)
library(slider)
library(purrr)
library(widyr)
library(furrr)
library(ggplot2)
#library(spacyr)
#spacy_initialize(model = "en_core_web_sm")

# load the coded and clean articles
articles <- read.csv(here::here("data/processed/article_text_codes_2024-09-10.csv"))

# remove/filter for titles with Please try again.
# select article text
articles_filter <- articles %>%
  filter(str_detect(Title.x, "Please try again.", negate = TRUE)) %>%
  select(Link, Article_Text)

# remove some entities
#text <- tolower(articles_filter$Article_Text)
#entitytxt <- spacy_extract_entity(text)
#head(entitytxt, n = 10)

#people_list <- entitytxt %>%
#  filter(ent_type == "PERSON") %>% 
#  group_by(text) %>%
#  summarise(count_person = n()) %>%
#  arrange(desc(count_person))


#stop_people <- paste(people_list$text, collapse = ",")
#stop_people_2 <- str_replace_all(stop_people, " ", ",")
#stop_people_list <- as.data.frame(strsplit(stop_people_2, ","))
#colnames(stop_people_list) <-("word")

#stop_people_list <- stop_people_list %>%
#  group_by(word) %>%
#  summarise(count_person = n())

# I want to remove words like wolf and bison from the people list
#word_to_keep <- as.data.frame(c("bison", "wolf", "mule", "deer", "gopher", "grizzly", "geese",
#                                "cow", "gator", "fox"))
#colnames(word_to_keep) <- ("word")
#stop_people_list <- stop_people_list %>%
#  anti_join(word_to_keep, by = "word")

# tokenize for sentences
# remove punctuation 
sentences <- articles_filter %>%
  unnest_tokens(sentence, Article_Text, token = "sentences")
sentences <- gsub('[[:punct:][:digit:] ]+',' ',sentences$sentence)

# Create two word embedding models
set.seed(2486)
model_sg <- word2vec(sentences, type = "skip-gram", dim = 15, iter = 20)
emb_sg <- as.matrix(model_sg)

model_cb <- word2vec(sentences, type = "cbow", dim = 15, iter = 20)
emb_cb <- as.matrix(model_cb)

vocab_sg <- summary(model_sg, type = "vocabulary")
vocab_cb <- summary(model_cb, type = "vocabulary")

vocab_sg_df <- as.data.frame(vocab_sg)


# tokenize for unigrams, remove stop words, and numbers
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

test_dict <- predict(model_cb, top_200_words$word, type = "nearest", top_n = 5)
test_dict_sg <- predict(model_sg, top_200_words$word, type = "nearest", top_n = 5)
test_dict[[1]]$term2
test_dict[[1]]
test_dict_sg[[1]]$term2
test_dict_sg[[1]]

test_dict_df <- data.frame()
for (i in 1:200){
  #print(i)
  tmp_words <- as.data.frame(test_dict[[i]]$term2)
  #print(tmp_words)
  test_dict_df <- rbind(test_dict_df, tmp_words)
}
colnames(test_dict_df) <-("word")
test_dict_df <- unique(test_dict_df)

test_dict_df <- full_join(test_dict_df, top_200_words, by = "word") %>%
  select(word) %>%
  unique(.)


## With (somewhat) optimized parameters
#### Create word embedding model
set.seed(2486)
model_sg <- word2vec(sentences, type = "skip-gram", dim = 50, 
                     window = 30, hs = FALSE, 
                     negative = 5, iter = 5)

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

### create a test dictionary - predict words using the word2vec model from the top 200 words
test_dict_nn <- predict(model_sg, top_200_words$word, type = "nearest", top_n = 5)
test_dict_emb_vector <- predict(model_sg, top_200_words$word, type = "embedding", top_n = 5)


test_dict_nn_df <- data.frame()
for (i in 1:200){
  #print(i)
  tmp_words <- as.data.frame(test_dict_nn[[i]]$term2)
  #print(tmp_words)
  test_dict_nn_df <- rbind(test_dict_nn_df, tmp_words)
}
colnames(test_dict_nn_df) <-("word")
test_dict_nn_df <- unique(test_dict_nn_df)



# look at different dictionaries
afinn <- get_sentiments("afinn")
afinn %>% filter(word == "wild")

bing <- get_sentiments("bing")
bing %>% filter(word == "wild")

nrc <- get_sentiments("nrc")
nrc %>% filter(word == "wild")

top_200_sentiments <- left_join(top_200_words, afinn, by = "word") %>%
  rename("afinn" = "value")
top_200_sentiments <- left_join(top_200_sentiments, bing, by = "word") 
top_200_sentiments <- left_join(top_200_sentiments, nrc, by = "word")
top_200_sentiments <- top_200_sentiments %>%
  rename("bing" = "sentiment.x", 
         "nrc" = "sentiment.y")

test_df_sentiments <- left_join(test_dict_nn_df, afinn, by = "word") %>%
  rename("afinn" = "value")
test_df_sentiments <- left_join(test_df_sentiments, bing, by = "word") %>%
  rename("bing" = "sentiment")
test_df_sentiments <- left_join(test_df_sentiments, nrc, by = "word") %>%
  rename("nrc" = "sentiment")

# make scatter plot
# from https://www.geeksforgeeks.org/word2vec-using-r/
# checking embeddings
skip_embedding <- as.matrix(model_sg)
skip_embedding <- predict(model_sg, top_200_words$word, type = "embedding")
skip_embedding <- na.omit(skip_embedding)

cbow_embedding <- as.matrix(model_cb)
cbow_embedding <- predict(model_cb, top_200_words$word, type = "embedding")
cbow_embedding <- na.omit(cbow_embedding)

library(ggplot2)
library(ggrepel)
library(plotly)
library(umap)
vizualization <- umap(skip_embedding, n_neighbors = 15, n_threads = 2)

df  <- data.frame(word = rownames(skip_embedding), 
                  xpos = gsub(".+//", "", rownames(skip_embedding)), 
                  x = vizualization$layout[, 1], y = vizualization$layout[, 2], 
                  stringsAsFactors = FALSE)

fig_sg <- plot_ly(df, x = ~x, y = ~y, type = "scatter", mode = 'text', text = ~word) %>%
  layout(title = "Embeddings Visualization: Skip-gram")

vizualization_cb <- umap(cbow_embedding, n_neighbors = 15, n_threads = 2)

df_cb  <- data.frame(word = rownames(cbow_embedding), 
                  xpos = gsub(".+//", "", rownames(cbow_embedding)), 
                  x = vizualization_cb$layout[, 1], y = vizualization_cb$layout[, 2], 
                  stringsAsFactors = FALSE)

fig_cb <- plot_ly(df_cb, x = ~x, y = ~y, type = "scatter", mode = 'text', text = ~word) %>%
  layout(title = "Embeddings Visualization: CBOW")

if (!require("processx")) install.packages("processx")
kaleido(fig_sg, "sg_embedding.svg")
kaleido(fig_cb, "cb_embedding.svg")

fig_sg
fig_cb
