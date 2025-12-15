################################################################################
# SCRIPT TO CLEAN AND LEMMATIZE TRAINING DATA SET                             ##
#
#
#
#
#
################################################################################

# 0. Load libraries
#-------------------------------------------------------------------------------
library(readr)
library(stringr)
#library(tidyverse)
#library(tidytext)
#library(SnowballC)
library(qdapDictionaries)
library(lubridate)
library(textstem)
library(textclean)
library(dplyr)
library(here)
library(tm)

# 1. Read in the data. Both the article text and the hand coded data, then join.
#-------------------------------------------------------------------------------
## Coded data
article_codes <- read.csv(file = here::here("data/original/james_edited_values_coding.csv"))
## Article text for the coded data
text_all <- read.csv(here::here("data/processed/article_text_2024-09-10.csv"))
#test_text_all <- read.csv(here::here("data/processed/clean_text_2025-05-13.csv"))
#test_text_all2 <- read.csv(here::here("data/processed/clean_text_2024-09-16.csv"))

## Join the text to the article codes
### Trim the white space from the article links/urls
article_codes$Link <- trimws(article_codes$Link)
text_all$Link <- trimws(text_all$Link)
### Join the data sets by the article links/urls
df_text_codes <- right_join(text_all, article_codes, by="Link")

# 2. Cleaning part 1
#-------------------------------------------------------------------------------
## Drop any rows with NA for Value_Orientation (the labels)
df_text_codes_clean1 <- df_text_codes[!is.na(df_text_codes$Value_Orientation), ]

## Drop any rows where the text is not available/
df_text_codes_clean1 <- df_text_codes_clean1 %>%
  filter(Article_Text != "Please try again. If the problem persists, contact Customer Service.") %>%
  filter(Article_Text != "Alerts will be sent when the new results matching your search criteria become available.")

## Clean up multiple-spellings in covariate columns
df_text_codes_clean1$Species <- str_replace(df_text_codes_clean1$Species, 'Grizzly Bears', 'Grizzly Bear')
df_text_codes_clean1$Focus <- str_replace(df_text_codes_clean1$Focus, 'Practicioner', 'Practitioner')
df_text_codes_clean1$Focus <- str_replace(df_text_codes_clean1$Focus, 'Practictioner', 'Practitioner')
df_text_codes_clean1$Focus <- str_replace(df_text_codes_clean1$Focus, 'Wildllife', 'Wildlife')
df_text_codes_clean1$Conflict_Type <- str_replace(df_text_codes_clean1$Conflict_Type, 'H-H', 'Human-Human')
df_text_codes_clean1$Conflict_Type <- str_replace(df_text_codes_clean1$Conflict_Type, 'H-W', 'Human-Wildlife')
df_text_codes_clean1$Conflict_Type <- str_replace(df_text_codes_clean1$Conflict_Type, 'N-W', 'Nature-Wildlife')
df_text_codes_clean1$Conflict_Type <- str_replace(df_text_codes_clean1$Conflict_Type, 'Unstated Conflict', 'Unstated')

## Convert the dates to datetime
df_text_codes_clean1 <- df_text_codes_clean1 %>%
  mutate(Published_Date = mdy(Published_Date))

## Drop any rows with no text in Article_Text
df_text_codes_clean1 <- df_text_codes_clean1 %>%
  filter(nchar(trimws(Article_Text)) > 0)

# 3. Cleaning part 2
#-------------------------------------------------------------------------------
## Source the custom functions

source(here::here("scripts/test/custom_cleaning_functions_LS.R"))

df_text_codes_clean2 <- df_text_codes_clean1 %>%
  mutate(
    # Ensure Article_Text column is class character
    Article_Text = as.character(Article_Text),
    # Use custom function to preserve abbreviatios
    Article_Text = preserve_abbreviations(Article_Text),
    # Custom gsub to remove urls/links
    Article_Text = gsub("\\b\\S*\\.(com|org|gov|edu|html|net)\\S*\\b", " ", Article_Text, ignore.case = TRUE),
    # Custom gsub to remove any emails
    Article_Text = gsub("\\S*@\\S*", " ", Article_Text, ignore.case = TRUE),
    # Custom gsub to remove digits connected to text
    Article_Text = gsub("\\S*\\d+\\S*", " ", Article_Text, ignore.case = TRUE),
    # Function for converting contractions (e.g. "don't" -> "do not")
    Article_Text = replace_contraction(Article_Text, ignore.case = TRUE),
    # Remove English stopwords
    Article_Text = gsub(paste0("\\b(", paste(stopwords("en"), collapse = "|"), ")\\b"), " ", Article_Text, ignore.case = TRUE),
    # Remove stopwords with additional stopwords dictionary
    Article_Text = gsub(paste0("\\b(", paste(BuckleySaltonSWL, collapse = "|"), ")\\b"), " ", Article_Text, ignore.case = TRUE),
    # Custom function for converting possessives
    Article_Text = convert_possessives(Article_Text),
    # Remove ordinal numbers
    Article_Text = replace_ordinal(Article_Text, num.paste = TRUE, remove = TRUE),
    # Remove numbers
    Article_Text = replace_number(Article_Text, num.paste = TRUE, remove = TRUE),
    # Ensure there is a space after every comma
    Article_Text = add_comma_space(Article_Text)
  )

## Save the cleaned text at this point
write_csv(df_text_codes_clean2, here::here(paste0("data/processed/df_text_codes_clean2_",
                                           Sys.Date(), ".csv")))

# 4. Create lemmatization dictionary
#-------------------------------------------------------------------------------
lemma_dict <- make_lemma_dictionary(
  df_text_codes_clean2$Article_Text,
  engine = "lexicon"
)

final_lemma_dict <- lemma_dict %>%
  filter(nchar(trimws(token)) > 1) # Keep rows with more than one character

str(final_lemma_dict)
head(final_lemma_dict)

# 5. Clean part 3
#-------------------------------------------------------------------------------

df_text_codes_clean3 <- df_text_codes_clean2 %>%
  mutate(
    # Lemmatize text (e.g. "abandoned" -> "abandon")
    Article_Text = lemmatize_strings(Article_Text, final_lemma_dict),
    # Remove all non letter characters (e.g. , : -)
    Article_Text = strip(Article_Text, char.keep = NULL, apostrophe.remove = TRUE),
    # Bring the abbreviations back to their original form
    Article_Text = gsub("DOTDOTDOT", ".", Article_Text, ignore.case = TRUE),
    # Remove leading or trailing white space
    Article_Text = stripWhitespace(Article_Text)
  )

head(df_text_codes_clean3)


