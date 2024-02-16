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

article_codes <- article_codes %>% 
  filter(!Link=='') %>% 
  mutate(id = row_number())

urls <- trimws(unique(article_codes$Link))

# set up a user agent so that the website doesn't think you are a robot
ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

df_article <- data.frame(Title=character(),
                         Author=character(),
                         Published_Date=character(),
                         Article_Text=character())

for (url in urls[1:2]){
  link <- url
  page <- GET(link, user_agent(ua))
  page_text <- httr::content(page, as = "text")
  tmp <- gsub(".*<h1", "", page_text) 
  article_title <- str_extract_all(tmp, regex("(?<=clipcopy\">).+?(?=</h1>)"))
  author <- str_extract_all(tmp, regex("(?<=Byline: ).+?(?=</span>)"))
  pub_date <- str_extract_all(tmp, regex("(?<=display-date\">).+?(?=</span>)"))
  tmp_noline <- str_remove_all(tmp, "\n")
  article_text <- str_extract(tmp_noline, regex("(?<=<p>).+?(?=</p>)"))
  df_article[nrow(df_article) + 1,] <- c(article_title, 
                                        author, 
                                        pub_date, 
                                        article_text)
}


## Breadcrumb for next time is to finish updating this code up to creating the data frame combining the new article code framework with the article text.


