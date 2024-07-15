library(stringr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(ggplot2)
library(tigris)

# Read in the data
article_codes <- read.csv(file = "data/original/new_article_coding.csv")

#text_grizz <- read.csv(here::here("data/processed/article_text_2024-04-04.csv"))
#text_coyote <- read.csv(here::here("data/processed/article_text_coyote_2024-04-08.csv"))
#text_boars <- read.csv(here::here("data/processed/article_text_boars_2024-04-08.csv"))
#text_wolf <- read.csv(here::here("data/processed/article_text_wolf_2024-04-08.csv"))
#text_beavs <- read.csv(here::here("data/processed/article_text_codes_beavers_2024-06-12.csv"))

#text_all <- rbind(text_grizz, text_beavs, text_boars, text_coyote, text_wolf)
text_all <- read.csv(here::here("data/processed/article_text_2024-07-11.csv"))


## join to the article codes
article_codes$Link <- trimws(article_codes$Link)
text_all$Link <- trimws(text_all$Link)

df_text_codes <- right_join(text_all, article_codes, by="Link")
df_text_codes <- df_text_codes %>%
  filter(grepl(" ", df_text_codes$Article_Text) == TRUE)
#df_text_codes <- df_text_codes %>%
#  filter(Species != "Bob Cats")

# Preprocess the data
## Clean up multiple-spellings in species names, focus, and conflict type
df_text_codes$Species <- str_replace(df_text_codes$Species, 'Grizzly Bears', 'Grizzly Bear')
df_text_codes$Focus <- str_replace(df_text_codes$Focus, 'Practicioner', 'Practitioner')
df_text_codes$Focus <- str_replace(df_text_codes$Focus, 'Practictioner', 'Practitioner')
df_text_codes$Focus <- str_replace(df_text_codes$Focus, 'Wildllife', 'Wildlife')
df_text_codes$Conflict_Type <- str_replace(df_text_codes$Conflict_Type, 'H-H', 'Human-Human')
df_text_codes$Conflict_Type <- str_replace(df_text_codes$Conflict_Type, 'H-W', 'Human-Wildlife')
df_text_codes$Conflict_Type <- str_replace(df_text_codes$Conflict_Type, 'N-W', 'Nature-Wildlife')
df_text_codes$Conflict_Type <- str_replace(df_text_codes$Conflict_Type, 'Unstated Conflict', 'Unstated')


#articles_spread <- df_text_codes %>%
#  group_by(Title.x) %>%
#  separate(Focus)

# Drop duplicated 
#df_text_codes <- df_text_codes[!duplicated(df_text_codes$Title.x), ]

## Clean up contractions
df_text_codes$Article_Text <- str_replace(df_text_codes$Article_Text, "we're", "we are")
df_text_codes$Article_Text <- str_replace(df_text_codes$Article_Text, "i'm", "i am")
df_text_codes$Article_Text <- str_replace(df_text_codes$Article_Text, "don't", "do not")
df_text_codes$Article_Text <- str_replace(df_text_codes$Article_Text, "it's", "it is")

df_text_codes <- df_text_codes %>%
  select(Title.x, Publication_State, Focus, Conflict_Type, Value_Orientation, Article_Text)

# save cleaned up data
write_csv(df_text_codes, here::here(paste0("data/processed/clean_text_", Sys.Date(), ".csv")), 
          col_names = TRUE)

## Tokenize
tidy_articles <- df_text_codes %>%
  unnest_tokens(word, Article_Text) %>% 
  filter(!grepl('[0-9]', word))  

## Check the most common words
tidy_articles %>%
  count(word, sort = TRUE) 

## Remove stop words
## Create a small data frame of your own stop words for this project 
#data("stop_words")
#grizz_stop_words <- data.frame(c("p", "br", "strong", "targetednews.com",
#                                 "grizzly", "grizzlies", "bears", "bear")) 
#colnames(grizz_stop_words) <-("word")

#tidy_articles <- tidy_articles %>%
#  anti_join(stop_words) %>%
#  anti_join(grizz_stop_words)

tidy_articles <- tidy_articles %>%
  select(Title.x, Focus, Conflict_Type, Value_Orientation, Publication_State, word)
