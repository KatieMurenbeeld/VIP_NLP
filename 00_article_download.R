library(httr)
library(tidyverse)
library(stringr)
library(googledrive)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
  )

# read in the csv with the urls for the articles
folder_url <- "https://drive.google.com/drive/u/0/folders/1ob5sagTtT3svhc7ZKeemd9TiAq1_MsCL"
folder <- drive_get(as_id(folder_url))

gdrive_files <- drive_ls(folder)
id <- gdrive_files[gdrive_files$name == "Article Coding", ]$id
drive_download(id, path = "data/article_coding.csv", overwrite = TRUE)

article_codes <- read.csv(file = 'data/article_coding.csv')

article_codes <- article_codes %>% 
  filter(!Link=='') %>% 
  mutate(id = row_number())
