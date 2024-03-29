library(stringr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(ggplot2)
library(tigris)

# Read in the data
article_codes <- read.csv(file = here::here("data/original/new_article_coding.csv"))
original_articles <- read.csv(here::here("data/processed/02_module_articles_2024-03-06.csv"))

# Exploratory Analysis

article_codes <- article_codes %>%
  drop_na(Value_Orientation)
  
length(unique(article_codes$Title))

article_codes %>%
  filter(Conflict_Type == "Human-Wildlife") %>%
  count(Focus)

group_mean<- aggregate(x= article_codes$Value_Orientation,
                       # Specify group indicator
                       by = list(article_codes$Conflict_Type),      
                       # Specify function (i.e. mean)
                       FUN = mean)
print(group_mean)

article_codes$Species <- str_replace(article_codes$Species, 'Grizzly Bears', 'Grizzly Bear')

n_species <- article_codes %>%
  group_by(Species) %>%
  count(Species, sort = TRUE) %>%
  ggplot(aes(Species, n, fill = Species)) + 
  geom_col() + 
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 22, angle = 60),
        axis.title.x = element_text(size = 24), 
        axis.text.y = element_text(size = 22)) 
n_species

ggsave(here::here("presentation/n_article_species.png"), n_species, width = 10, height = 14, dpi = 300)  


misspelling <- c('Practicioner','Practioner', 'Practictioner', 'Practioners', 'Practitioners')

for (focus in misspelling) {
article_codes$Focus <- str_replace(article_codes$Focus, focus, 'Practitioner')
}
article_codes$Focus <- trimws(str_replace(article_codes$Focus, 'Wildllife', 'Wildlife'))

n_focus <- article_codes %>%
  group_by(Focus) %>%
  count(Focus, sort = TRUE) %>%
  ggplot(aes(Focus, n, fill = Focus)) + 
  geom_col() + 
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 22, angle = 60),
        axis.title.x = element_text(size = 24), 
        axis.text.y = element_text(size = 22)) 
n_focus

ggsave(here::here("presentation/n_article_focus.png"), n_focus, width = 10, height = 14, dpi = 300)  

article_codes$Conflict_Type <- trimws(article_codes$Conflict_Type)
article_codes$Conflict_Type <- str_replace(article_codes$Conflict_Type, 'H-H', 'Human-Human')
article_codes$Conflict_Type <- str_replace(article_codes$Conflict_Type, 'H-W', 'Human-Wildlife')
article_codes$Conflict_Type <- str_replace(article_codes$Conflict_Type, 'N-W', 'Nature-Wildlife')
article_codes$Conflict_Type <- trimws(str_replace(article_codes$Conflict_Type, 'Conflict', ''))

n_conflict <- article_codes %>%
  group_by(Conflict_Type) %>%
  count(Conflict_Type, sort = TRUE) %>%
  ggplot(aes(Conflict_Type, n, fill = Conflict_Type)) + 
  geom_col() + 
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 22),
        axis.title.x = element_text(size = 24), 
        axis.text.y = element_text(size = 22)) 
n_conflict 

ggsave(here::here("presentation/n_article_conflict.png"), n_conflict, width = 10, height = 14, dpi = 300)  
  

article_codes %>%
  group_by(Title) %>%
  summarise(mean_vo = mean(Value_Orientation)) %>%
  count(mean_vo, sort = TRUE) %>%
  ggplot(aes(mean_vo)) + 
  geom_density()

article_codes %>%
  group_by(Publication_State) %>%
  count(Publication_State, sort = TRUE)

# Create a map showing number of articles per state
## Create data frame

state_df <- article_codes %>%
  drop_na(Value_Orientation) %>%
  group_by(Publication_State) %>%
  summarise(mean_value = mean(Value_Orientation), n = n(), n_article = length(unique(Title)))

## Create state map variable
us_states <- states(cb = TRUE) %>%
  filter(GEOID < "60") %>%
  filter(GEOID != "02") %>%
  filter(GEOID != "15") 

## Join the 2 data frames and replace mean_values of NA with 0s
state_val <- right_join(state_df, us_states, by = c("Publication_State" = "STUSPS"))


n_article_map <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = state_val, aes(geometry = geometry, fill = n_article), size = 0.05) +
  labs(title = "Total Number of Articles") +
  # scale_fill_discrete(name = "Mean Value Orientation") +
  theme(plot.title = element_text(size=12),
        legend.title = element_text(size=10)) +
  theme_bw()
n_article_map

ggsave(here::here("presentation/n_article_map.png"), n_article_map, width = 14, height = 14, dpi = 300) 

# Text Analysis
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
#write.csv(articles_joined, here::here("data/processed/03_joined-article_code_text.csv"), row.names = FALSE)

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

all_word <- tidy_articles %>%
  count(word, sort = TRUE) %>%
  filter(n > 250) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 24, angle = 90),
        axis.text.y = element_text(size = 24))

ggsave(here::here("presentation/all_common_words.png"), width = 15, height = 12, dpi = 300)

## Can also plot by species

unique(tidy_articles$Species) # Need to clean up names

### Can enter your species of interest here
species <- "Boars"

species_words <- tidy_articles %>%
  filter(Species %in% species) %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=10) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  labs(x = NULL) + 
  ggtitle(paste(species, "10 most common words")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 24, angle = 90),
        axis.text.y = element_text(size = 24))

ggsave(here::here(paste0("presentation/", species, "_common_words.png")), species_words, width = 15, height = 12, dpi = 300)

species_stems <- tidy_articles %>%
  filter(Species %in% species) %>%
  count(stem, sort = TRUE) %>%
  slice_max(n, n=10) %>%
  filter(n > 5) %>%
  mutate(stem = reorder(stem, n)) %>%
  ggplot(aes(stem, n)) +
  geom_col() +
  labs(x = NULL) +  
  ggtitle(paste(species, "10 most common stems"))  +
  theme_bw() +
  theme(axis.text.x = element_text(size = 24, angle = 90),
        axis.text.y = element_text(size = 24))

ggsave(here::here(paste0("presentation/", species, "_common_stems.png")), species_stems, width = 15, height = 12, dpi = 300)


## Could also plot by Focus or any other variable

unique(tidy_articles$Focus) # Need to clean up names

focus <- c("People")

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

