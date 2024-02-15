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
drive_download(id, path = "data/new_article_coding.csv", overwrite = TRUE)

article_codes <- read.csv(file = 'data/new_article_coding.csv')
folder_url <- "https://drive.google.com/drive/u/0/folders/1ob5sagTtT3svhc7ZKeemd9TiAq1_MsCL"
folder <- drive_get(as_id(folder_url))

gdrive_files <- drive_ls(folder)
id <- gdrive_files[gdrive_files$name == "Article Coding", ]$id
drive_download(id, path = "data/article_coding.csv", overwrite = TRUE)

article_codes <- read.csv(file = 'data/article_coding.csv')

article_codes <- article_codes %>% 
  filter(!Link=='') %>% 
  mutate(id = row_number())

urls <- unique(article_codes$Link)

link <- urls[1]
page <- GET(link)
page_text <- httr::content(page, as = 'text')

# Testing out with rvest
page_html <- read_html(link)
  
# full Xpath to article content
# Title
# /html/body/div[2]/div[3]/section[2]/main/div[2]/div/div[1]/h1
# Newspaper info
# /html/body/div[2]/div[3]/section[2]/main/div[2]/div/div[1]/div[1]/div[1]
# Author byline
# /html/body/div[2]/div[3]/section[2]/main/div[2]/div/div[1]/div[1]/div[2]
# Article text
# /html/body/div[2]/div[3]/section[2]/main/div[2]/div/div[1]/div[3]/p












=======
urls <- article_codes$Link

link <- urls[1]
page <- GET(link)
page_text <- content(page, as = 'text')
  
  
  
>>>>>>> refs/remotes/origin/main
