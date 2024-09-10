library(stringr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(ggplot2)
library(tigris)
library(lubridate)

# Read in the data
article_codes <- read.csv(file = "data/original/james_edited_values_coding.csv")
text_all <- read.csv(here::here("data/processed/article_text_2024-09-10.csv"))


## join to the article codes
article_codes$Link <- trimws(article_codes$Link)
text_all$Link <- trimws(text_all$Link)

df_text_codes <- right_join(text_all, article_codes, by="Link")
df_text_codes <- df_text_codes %>%
  filter(grepl(" ", df_text_codes$Article_Text) == TRUE)

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

## Clean up contractions
df_text_codes$Article_Text <- str_replace(df_text_codes$Article_Text, "we're", "we are")
df_text_codes$Article_Text <- str_replace(df_text_codes$Article_Text, "i'm", "i am")
df_text_codes$Article_Text <- str_replace(df_text_codes$Article_Text, "don't", "do not")
df_text_codes$Article_Text <- str_replace(df_text_codes$Article_Text, "it's", "it is")

## Get the minimum and maximum date
colnames(df_text_codes)

### convert Published_Date to datetime
df_text_codes <- df_text_codes %>%
  mutate(Published_Date = mdy(Published_Date))

min(df_text_codes$Published_Date, na.rm = TRUE)
max(df_text_codes$Published_Date, na.rm = TRUE)

## Get the unique sources
unique(df_text_codes$Source)
length(unique(df_text_codes$Source))
sources <- as.data.frame(df_text_codes %>% 
                           distinct(Link, .keep_all = TRUE) %>%
                           group_by(Source) %>%
                           summarise(count = n()))

# Select columns of interest
df_text_codes <- df_text_codes %>%
  select(Title.x, Publication_State, Focus, Conflict_Type, Value_Orientation, Article_Text)

# save cleaned up data
write_csv(df_text_codes, here::here(paste0("data/processed/clean_text_", Sys.Date(), ".csv")), 
          col_names = TRUE)
