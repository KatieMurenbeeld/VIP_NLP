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

# set up a user agent so that the website doesn't think you are a robot
ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

link <- urls[1]
page <- GET(link, user_agent(ua))
page_text <- httr::content(page, as = "text")
text_body <- str_extract_all(page_text, regex("(?<=<p>).+(?=</p>)", multiline = TRUE), simplify = TRUE) 
text <- text_body[[2]]

str_extract_all(page_text, regex("(?<=<h1).+(?=</h1>)"), simplify = TRUE) 
str_extract(page_text, regex("(?<=<h1).*?(?=</h1>)")) 

str_extract_all(page_text, regex("(?<=<p>).+"), simplify = TRUE)
test <- str_extract_all(page_text, regex("(?<=<p>).+", multiline = TRUE), simplify = TRUE) # only goes up to new line break of paragraph that I want.
str_count(page_text, "<p>")

page_text_noline <- str_remove_all(page_text, "\n")

str_extract_all(page_text_noline, regex("(?<=<p>).+"), simplify = TRUE) 

## Breadcrumb for next time is to figure out the regular expression.

##############################
# Testing out with rvest
page_html <- read_html(link, user_agent = ua)
  
# full Xpath to article content
# Title
# /html/body/div[2]/div[3]/section[2]/main/div[2]/div/div[1]/h1
# Newspaper info
# /html/body/div[2]/div[3]/section[2]/main/div[2]/div/div[1]/div[1]/div[1]
# Author byline
# /html/body/div[2]/div[3]/section[2]/main/div[2]/div/div[1]/div[1]/div[2]
# Article text
# /html/body/div[2]/div[3]/section[2]/main/div[2]/div/div[1]/div[3]/p
# //*[@id="document-view--ascii"]/div/div[1]/div[3]/p
test_p <- page_html %>%
  html_nodes(xpath = '//*[@id="document-view--ascii"]/div/div[1]/div[3]/p') %>%
  html_text2()

