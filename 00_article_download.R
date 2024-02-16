library(httr)
library(tidyverse)
library(stringr)
library(rvest)
library(googledrive)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
  )

# read in the csv with the urls for the articles
folder_url <- "https://drive.google.com/drive/folders/1ob5sagTtT3svhc7ZKeemd9TiAq1_MsCL"
## old folder url, not sure if it will make a difference
#folder_url <- "https://drive.google.com/drive/u/0/folders/1ob5sagTtT3svhc7ZKeemd9TiAq1_MsCL"
folder <- drive_get(as_id(folder_url))

gdrive_files <- drive_ls(folder)
id <- gdrive_files[gdrive_files$name == "New Article Coding Framework", ]$id
drive_download(id, path = "data/original/new_article_coding.csv", overwrite = TRUE)

article_codes <- read.csv(file = "data/original/new_article_coding.csv")

# Create a unique article ID
article_codes <- article_codes %>% 
  filter(!Link=='') %>%
  mutate(tmp = match(Link, unique(Link)),
         ID = sprintf('Article%003d', tmp)) %>%
  select(-tmp)
# Save as a new csv in the data/processed folder
write.csv(article_codes, "data/processed/new_article_coding_ids.csv")


