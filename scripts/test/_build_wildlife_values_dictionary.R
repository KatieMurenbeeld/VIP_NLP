library(word2vec)
library(tidyverse)
library(tidytext)
library(slider)
library(purrr)
library(widyr)
library(furrr)
library(ggplot2)
library(SnowballC)
library(forcats)
library(stringr)
library(sentimentr)

# load the coded and clean articles
articles <- read.csv(here::here("data/processed/article_text_codes_2024-09-10.csv"))

# filter out articles that have a value orientation of 1 (mutual) or 
# a value orientation of 7 (domination)

# I think I want to combine (concatenate) all the text together for 
# each value orientation
mutual <- articles %>%
  filter(Value_Orientation == 1) %>%
  select(Value_Orientation, Article_Text, Species)
dominate <- articles %>%
  filter(Value_Orientation == 7) %>%
  select(Value_Orientation, Article_Text, Species)

# get the species for each set of articles and add to a stop words dictionary
unique(mutual$Species)
unique(dominate$Species)

# create stop words dictionary
data("stop_words")

my_stop_words <- data.frame(c("pig", "pigs", "bear", "bears", 
                              "hog", "hogs", "beaver", "beavers", 
                              "boar", "boars", "coyote", "coyotes", 
                              "monarch", "butterfly", "butterflies",
                              "lynx", "raccoon", "raccoons", "elk",
                              "mink", "alligator", "alligators", 
                              "porcupine", "porcupines", "pronghorn", 
                              "pronghorns", "rabbit", "rabbits", "prairie",
                              "dog", "dogs", "wolf", "wolves", "bison", "grizzly",
                              "grizzlies", "lion", "lions", "cat", "cats", 
                              "wild", "wildlife", "animal", "animals")) #remove 
colnames(my_stop_words) <-("word")




# combine all the text for each value orientation
mutual_cat <- mutual %>%
  select(-Species) %>%
  summarise(alltext = paste(Article_Text, collapse = " "),
            Value_Orientation = mean(Value_Orientation))

dominate_cat <- dominate %>%
  select(-Species) %>%
  summarise(alltext = paste(Article_Text, collapse = " "),
            Value_Orientation = mean(Value_Orientation))

ext_cat <- rbind(dominate_cat, mutual_cat)

# also want to remove people's names
# load and initialize the spacyr library
library(spacyr)
#spacy_install()
spacy_initialize(model = "en_core_web_sm")

text <- ext_cat$alltext

parsedtxt <- spacy_parse(text)

entitytxt <- spacy_extract_entity(text)

ext_cat_people <- entitytxt %>%
  filter(ent_type == "PERSON") %>% 
  group_by(doc_id, text) %>%
  summarise(count_person = n()) %>%
  arrange(desc(count_person))

# from ext_cat_people create a list of names
# get values from text, separate and put into list

stop_people <- paste(ext_cat_people$text, collapse = ",")
stop_people_list <- as.data.frame(strsplit(stop_people, ","))
colnames(stop_people_list) <-("word")
stop_people_list$word <- tolower(stop_people_list$word)


# tokenize and get term counts
all_words <- articles %>%
  unnest_tokens(word, Article_Text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  anti_join(stop_people_list, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE)
  
  
ext_words <- ext_cat %>%
  unnest_tokens(word, alltext) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  anti_join(stop_people_list, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(Value_Orientation, stem, sort = TRUE)


total_words <- ext_words %>%
  group_by(Value_Orientation) %>%
  summarise(total = sum(n))

ext_words <- left_join(ext_words, total_words)
head(ext_words)


# plot the most common words for each orientation

ext_words %>%
  group_by(Value_Orientation) %>%
  slice_max(n, n = 40) %>%
  ungroup() %>%
  ggplot(aes(n, fct_reorder(stem, n), fill = Value_Orientation)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Value_Orientation, ncol = 2, scales = "free") +
  labs(x = "word count", y = NULL)

ext_words %>%
  filter(Value_Orientation == 1) %>%
  slice_max(n, n = 30) %>%
  ggplot(aes(n, fct_reorder(stem, n))) +
  geom_col(show.legend = FALSE) +
  labs(x = "mutualism - word count", y = NULL)

ext_words %>%
  filter(Value_Orientation == 7) %>%
  slice_max(n, n = 30) %>%
  ggplot(aes(n, fct_reorder(stem, n), fill = Value_Orientation)) +
  geom_col(show.legend = FALSE) +
  labs(x = "domination - word count", y = NULL)

all_words %>%
  slice_max(n, n = 30) %>%
  ggplot(aes(n, fct_reorder(stem, n))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Top 30 Words: All Coded Articles", y = NULL)

# calculate tf-idf
ext_tf_idf <- ext_words %>%
  bind_tf_idf(stem, Value_Orientation, n)
ext_tf_idf

ext_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

ext_tf <- ext_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf))

# plot the words with the highest tf_idf for each orientation

ext_tf_idf %>%
  group_by(Value_Orientation) %>%
  slice_max(tf_idf, n = 40) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(stem, tf_idf), fill = Value_Orientation)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Value_Orientation, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

ext_tf %>%
  group_by(Value_Orientation) %>%
  slice_max(tf, n = 40) %>%
  ungroup() %>%
  ggplot(aes(tf, fct_reorder(stem, tf), fill = Value_Orientation)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Value_Orientation, ncol = 2, scales = "free") +
  labs(x = "tf", y = NULL)

# repeat for bigrams
# tokenize and get term counts

ext_bi <- ext_cat %>%
  unnest_tokens(bigram, alltext, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# filter stop words
# calculate tf-idf
ext_bi_tf_idf <- ext_bi %>%
  count(Value_Orientation, bigram) %>%
  bind_tf_idf(bigram, Value_Orientation, n) %>%
  arrange(desc(tf_idf))
ext_bi_tf_idf

ext_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# plot the words with the highest tf_idf for each orientation

ext_bi_tf_idf %>%
  group_by(Value_Orientation) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Value_Orientation)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Value_Orientation, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


# look at different dictionaries
afinn <- get_sentiments("afinn")
afinn %>% filter(word == "parasite")
