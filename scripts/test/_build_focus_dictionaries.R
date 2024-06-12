library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(discrim)
library(naivebayes)
library(recipes)
library(hardhat)
library(glmnet)
library(utiml)
library(tm)
library(caret)


# Load the cleaned data
#articles_text_clean <- read_csv(here::here("data/processed/clean_text_2024-04-09.csv"))
articles_text_clean <- read_csv(here::here("data/processed/article_text_codes_beavers_2024-06-12.csv"))

text_clean_sel <- articles_text_clean %>%
  dplyr::select(Title.y, Publication_State, Link, Article_Text, Focus, Conflict_Type, Value_Orientation)

## Clean up remaining html code
#cleanFun <- function(htmlString) {
#  return(gsub("<.*?>", "", htmlString))
#}

#cleanFun2 <- function(htmlString) {
#  return(gsub(">.*?</", "", htmlString))
#}

#for (i in 1:length(articles_text_clean)) {
#  articles_text_clean[[6]][i] <- cleanFun(articles_text_clean[[6]][i])
#  articles_text_clean[[6]][i] <- cleanFun2(articles_text_clean[[6]][i])
#}

# Create simplified value_orientation
text_clean_sel$value_simple <- as.factor(text_clean_sel$Value_Orientation)
text_clean_sel=within(text_clean_sel,{
  value_simple=NA
  value_simple[Value_Orientation == "1" | Value_Orientation == "2"] = "Mutualistic"
  value_simple[Value_Orientation == "3" | Value_Orientation == "4" | Value_Orientation == "5"] = "Neutral"
  value_simple[Value_Orientation == "6" | Value_Orientation == "7"] = "Domination"
})

text_clean_sel$Focus <- str_replace(text_clean_sel$Focus, 'Practicioner', 'Practitioner')
text_clean_sel$Focus <- str_replace(text_clean_sel$Focus, 'Practictioner', 'Practitioner')
text_clean_sel$Focus <- str_replace(text_clean_sel$Focus, 'Wildllife', 'Wildlife')
text_clean_sel$Conflict_Type <- str_replace(text_clean_sel$Conflict_Type, 'H-H', 'Human-Human')
text_clean_sel$Conflict_Type <- str_replace(text_clean_sel$Conflict_Type, 'H-W', 'Human-Wildlife')
text_clean_sel$Conflict_Type <- str_replace(text_clean_sel$Conflict_Type, 'N-W', 'Nature-Wildlife')
text_clean_sel$Conflict_Type <- str_replace(text_clean_sel$Conflict_Type, 'Unstated Conflict', 'Unstated')

# tokenize: unigram, bi-gram, and sentences
tidy_text_uni <-  text_clean_sel %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word))  

tidy_text_bi <-  text_clean_sel %>%
  unnest_tokens(bigram, Article_Text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  filter(!grepl('[0-9]', bigram))  

tidy_text_sent <- text_clean_sel %>%
  unnest_tokens(sentence, Article_Text, token = "sentences") %>%
  filter(!grepl('[0-9]', sentence))

## Remove stop words
## Create a small data frame of your own stop words for this project 
data("stop_words")
wildlife_stop_words <- data.frame(c("p", "br", "strong", "targetednews.com",
                                    "grizzly", "grizzlies", "bears", "bear", 
                                    "wolf", "wolves", "coyote", "coyotes", 
                                    "pigs", "pig", "beaver", "beavers", 
                                    "amp", "div", "class", "span", "href",
                                    "wildlife", "wild", "fish", "animal",
                                    "animals", "species")) 
colnames(wildlife_stop_words) <-("word")

uni_text_stop <- tidy_text_uni %>%
  anti_join(stop_words) %>%
  anti_join(wildlife_stop_words)

bigrams_separated <- tidy_text_bi %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% wildlife_stop_words$word) %>%
  filter(!word2 %in% wildlife_stop_words$word)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# I want to separate out into separate dataframes for each focus

uni_policy <- uni_text_stop %>%
  filter(Focus == "Policy")

uni_people <- uni_text_stop %>%
  filter(Focus == "People")

uni_prac <- uni_text_stop %>%
  filter(Focus == "Practitioner")

uni_wild <- uni_text_stop %>%
  filter(Focus == "Wildlife")

uni_eco <- uni_text_stop %>%
  filter(Focus == "Ecosystem")

bi_policy <- bigrams_united %>%
  filter(Focus == "Policy")

bi_people <- bigrams_united %>%
  filter(Focus == "People")

bi_prac <- bigrams_united %>%
  filter(Focus == "Practitioner")

bi_wild <- bigrams_united %>%
  filter(Focus == "Wildlife")

bi_eco <- bigrams_united %>%
  filter(Focus == "Ecosystem")

sent_policy <- tidy_text_sent %>%
  filter(Focus == "Policy")

sent_people <- tidy_text_sent %>%
  filter(Focus == "People")

sent_prac <- tidy_text_sent %>%
  filter(Focus == "Practitioner")

sent_wild <- tidy_text_sent %>%
  filter(Focus == "Wildlife")

sent_eco <- tidy_text_sent %>%
  filter(Focus == "Ecosystem")

## Common words and word-pairings per Focus area
focus_words <- uni_text_stop %>%
  count(Focus, word)

total_words <- focus_words %>% 
  group_by(Focus) %>% 
  summarize(total = sum(n))

focus_words <- left_join(focus_words, total_words)

focus_bigrams <- bigrams_united %>%
  count(Focus, bigram)

total_bigrams <- focus_bigrams %>% 
  group_by(Focus) %>% 
  summarize(total = sum(n))

focus_bigrams <- left_join(focus_bigrams, total_bigrams)

# Plot

focus_words %>%
  filter(Focus == "People") %>%
  filter(n > 12) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "lightblue", alpha = 0.8) +
  theme_bw() +
  labs(y = NULL)

focus_words %>%
  filter(Focus == "Wildlife") %>%
  filter(n > 12) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "tan", alpha = 0.8) +
  labs(y = NULL)

focus_words %>%
  filter(Focus == "Policy") %>%
  filter(n > 12) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "orange", alpha = 0.8) +
  theme_bw() +
  labs(y = NULL)

focus_words %>%
  filter(Focus == "Practitioner") %>%
  #filter(n > 12) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

focus_words %>%
  filter(Focus == "Ecosystem") %>%
  filter(n > 12) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "lightgreen", alpha = 0.8) +
  labs(y = NULL)

focus_bigrams %>%
  filter(Focus == "People") %>%
  filter(n > 2) %>%
  ggplot(aes(n, bigram)) +
  geom_col(fill = "lightblue", alpha = 0.8) +
  theme_bw() +
  labs(y = NULL)

focus_bigrams %>%
  filter(Focus == "Wildlife") %>%
  filter(n > 2) %>%
  ggplot(aes(n, bigram)) +
  geom_col(fill = "tan", alpha = 0.8) +
  labs(y = NULL)

focus_bigrams %>%
  filter(Focus == "Policy") %>%
  filter(n > 2) %>%
  ggplot(aes(n, bigram)) +
  geom_col(fill = "orange", alpha = 0.8) +
  theme_bw() +
  labs(y = NULL)

focus_bigrams %>%
  filter(Focus == "Practitioner") %>%
  filter(n > 2) %>%
  ggplot(aes(n, bigram)) +
  geom_col() +
  labs(y = NULL)

focus_bigrams %>%
  filter(Focus == "Ecosystem") %>%
  filter(n > 2) %>%
  ggplot(aes(n, bigram)) +
  geom_col(fill = "lightgreen", alpha = 0.8) +
  labs(y = NULL)

# Could I build a focus term matrix? Yes!
## Common words and word-pairings per Focus area
focus_words <- uni_text_stop %>%
  count(Focus, word)

total_words <- focus_words %>% 
  group_by(Focus) %>% 
  summarize(total = sum(n))

focus_words <- left_join(focus_words, total_words)

focus_tf_idf <- focus_words %>%
  bind_tf_idf(word, Focus, n)

focus_tf_idf %>%
  group_by(Focus) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Focus)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Focus, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# repeat with bigrams
focus_bigrams <- bigrams_united %>%
  count(Focus, bigram)

total_bigrams <- focus_bigrams %>% 
  group_by(Focus) %>% 
  summarize(total = sum(n))

focus_bigrams <- left_join(focus_bigrams, total_bigrams)

focus_bi_tf_idf <- focus_bigrams %>%
  bind_tf_idf(bigram, Focus, n)

focus_bi_tf_idf %>%
  group_by(Focus) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Focus)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Focus, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

focus_bi_tf_idf %>%
  filter(Focus == "Practitioner") %>%
  slice_max(tf_idf, n = 9) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Focus)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL)

# test with value orientation
value_words <- uni_text_stop %>%
  count(value_simple, word)

total_words <- value_words %>% 
  group_by(value_simple) %>% 
  summarize(total = sum(n))

value_words <- left_join(value_words, total_words)

value_tf_idf <- value_words %>%
  bind_tf_idf(word, value_simple, n)

value_tf_idf %>%
  group_by(value_simple) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = value_simple)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value_simple, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

value_bigrams <- bigrams_united %>%
  count(value_simple, bigram)

total_bigrams <- value_bigrams %>% 
  group_by(value_simple) %>% 
  summarize(total = sum(n))

value_bigrams <- left_join(value_bigrams, total_bigrams)

value_bi_tf_idf <- value_bigrams %>%
  bind_tf_idf(bigram, value_simple, n)

value_bi_tf_idf %>%
  group_by(value_simple) %>%
  slice_max(tf_idf, n = 7) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = value_simple)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value_simple, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# repeat with conflict type
conflict_bigrams <- bigrams_united %>%
  count(Conflict_Type, bigram)

total_bigrams <- conflict_bigrams %>% 
  group_by(Conflict_Type) %>% 
  summarize(total = sum(n))

conflict_bigrams <- left_join(conflict_bigrams, total_bigrams)

conflict_bi_tf_idf <- conflict_bigrams %>%
  bind_tf_idf(bigram, Conflict_Type, n)

conflict_bi_tf_idf %>%
  group_by(Conflict_Type) %>%
  slice_max(tf_idf, n = 7) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Conflict_Type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Conflict_Type, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)




