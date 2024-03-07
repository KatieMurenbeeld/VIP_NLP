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

# can also look at sentences
tidy_sentences <- joined_articles %>%
  unnest_tokens(sentences, full_text, token = "sentences") %>%
  filter(!is.na(sentences)) %>%
  mutate(sentenceID = 1:n()) %>%
  unnest_tokens(word, sentences, 'words') %>%
  anti_join(stop_words) %>%
  anti_join(my_stop_words)

# create a data frame with 3 different sentiment lexicons

df_bing <- tidy_articles %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

species <- c("Bob Cats")

df_bing_species <- tidy_articles %>%
  filter(Species %in% species) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() 

focus <- c("Practitioner")

df_bing_focus <- tidy_articles %>%
  filter(Focus %in% focus) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() 

#-----

df_afinn <- tidy_articles %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  ungroup()

df_afinn_species <- tidy_articles %>%
  filter(Species %in% species) %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  ungroup()

df_afinn_focus <- tidy_articles %>%
  filter(Focus %in% focus) %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  ungroup()

#----

df_nrc <- tidy_articles %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

df_nrc_species <- tidy_articles %>%
  filter(Species %in% species) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

df_nrc_focus <- tidy_articles %>%
  filter(Focus %in% focus) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#----

df_afinn_sent <- tidy_sentences %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(doc_id, sentenceID) %>% 
  summarise(sentiment = sum(value),
            dom_mut = mean(Value_Orientation)) %>%
  group_by(doc_id) %>%
  mutate(method = "AFINN") %>%
  ungroup()

df_bing_sent <- tidy_sentences %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(doc_id, sentenceID) %>% 
  summarise(dom_mut = mean(Value_Orientation)) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(doc_id) %>%
  mutate(method = "Bing") %>%
  ungroup()

# Plot the sentiments
all_sentiment <- df_bing %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  scale_fill_manual(values=c("darkgrey", "lightyellow")) + # change the colors of the bar plot
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24))

ggsave(here::here("presentation/all_sentiment.png"), all_sentiment, width = 12, height = 15, dpi = 300)

species_sent <- df_bing_species %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  scale_fill_manual(values=c("darkgrey", "lightyellow")) + # change the colors of the bar plot
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL, 
       title = paste0("Sentiment Contribute: ", species))  +
  theme_bw() +
  theme(axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24))

ggsave(here::here(paste0("presentation/", species, "_sentiment.png")), species_sent, width = 12, height = 15, dpi = 300)


df_bing_focus %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  scale_fill_manual(values=c("darkgrey", "lightyellow")) + # change the colors of the bar plot
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL, 
       title = paste0("Sentiment Contribute: ", focus))

# make line plots

# compare sentiments to value orientation
## but are they a one to one comparison? 
## who assumes that domination is bad and mutualism is good?

# compare the various sentiments to the value orientation






