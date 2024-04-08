library(stringr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(ggplot2)
library(tigris)

# Read in the data
text_codes <- read.csv(here::here("data/processed/article_text_codes_2024-04-04.csv"))

# Preprocess the data
articles_text <- text_codes %>%
  filter(Species == "Grizzly Bear" | Species == "Grizzly Bears")

## Clean up multiple-spellings in species names, focus, and conflict type
articles_text$Species <- str_replace(articles_text$Species, 'Grizzly Bears', 'Grizzly Bear')
articles_text$Focus <- str_replace(articles_text$Focus, 'Practicioner', 'Practitioner')
articles_text$Conflict_Type <- str_replace(articles_text$Conflict_Type, 'H-H', 'Human-Human')
articles_text$Conflict_Type <- str_replace(articles_text$Conflict_Type, 'H-W', 'Human-Wildlife')
articles_text$Conflict_Type <- str_replace(articles_text$Conflict_Type, 'N-W', 'Nature-Wildlife')
articles_text$Conflict_Type <- str_replace(articles_text$Conflict_Type, 'Unstated Conflict', 'Unstated')


#articles_spread <- articles_text %>%
#  group_by(Title.x) %>%
#  separate(Focus)

# Drop duplicated 
#articles_text <- articles_text[!duplicated(articles_text$Title.x), ]

## Clean up contractions
articles_text$Article_Text <- str_replace(articles_text$Article_Text, "we're", "we are")
articles_text$Article_Text <- str_replace(articles_text$Article_Text, "i'm", "i am")
articles_text$Article_Text <- str_replace(articles_text$Article_Text, "don't", "do not")
articles_text$Article_Text <- str_replace(articles_text$Article_Text, "it's", "it is")

articles_text <- articles_text %>%
  select(Title.x, Publication_State, Focus, Conflict_Type, Value_Orientation, Article_Text)

# save cleaned up data
write_csv(articles_text, here::here(paste0("data/processed/grizz_clean_text_", Sys.Date(), ".csv")), 
          col_names = TRUE)

## Tokenize
tidy_articles <- articles_text %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word))  

## Check the most common words
tidy_articles %>%
  count(word, sort = TRUE) 

## Remove stop words
## Create a small data frame of your own stop words for this project 
data("stop_words")
grizz_stop_words <- data.frame(c("p", "br", "strong", "targetednews.com",
                                 "grizzly", "grizzlies", "bears", "bear")) 
colnames(grizz_stop_words) <-("word")

tidy_articles <- tidy_articles %>%
  anti_join(stop_words) %>%
  anti_join(grizz_stop_words)

tidy_articles <- tidy_articles %>%
  select(Title, Focus, Conflict_Type, Value_Orientation, Publication_State, word)
