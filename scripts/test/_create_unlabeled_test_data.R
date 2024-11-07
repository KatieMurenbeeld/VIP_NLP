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
folder <- drive_get(as_id(folder_url))

gdrive_files <- drive_ls(folder)
id <- gdrive_files[gdrive_files$name == "newdata_for_model_testing", ]$id
drive_download(id, path = "data/original/newdata_for_model_testing.csv", overwrite = TRUE)

article_codes <- read.csv(file = "data/original/newdata_for_model_testing.csv")

urls <- trimws(unique(article_codes$Link))

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

df_article <- data.frame(Title = character(),
                         Author = character(),
                         Published_Date = character(),
                         Source = character(),
                         Article_Text = character(), 
                         Link = character())

for (url in urls){
  link <- url
  page <- GET(link, user_agent(ua))
  page_text <- read_html(page) 
  article_title <- page_text %>% html_element("h1") %>% html_text2()
  article_text_all <- page_text %>% html_elements("p") %>% html_text2()
  if(length(article_title) == 0){
    article_title <- "none given"
  } else if (is.na(article_title) == TRUE){
    article_title <- article_text_all[1]
  } else if (article_title == ""){
    article_title <- article_text_all[9]
  } else{article_title <- article_title}
  author <- page_text %>% html_nodes("span.author-byline") %>% html_text2()
  if(length(author) == 0){
    author <- "none given"
  } else{author <- author}
  pub_date <- page_text %>% html_nodes("span.display-date") %>% html_text2()
  if(length(pub_date) == 0){
    pub_date <- "none given"
  } else{pub_date <- pub_date}
  source <- page_text %>% html_nodes("span.source") %>% html_text2()
  if(length(source) == 0){
    source <- "none given"
  } else{source <- source}
  article_text <- article_text_all[9]
  if(length(article_text) == 0){
    article_text <- "pdf of actual newspaper"
  } else if (is.na(article_text) == TRUE){
    article_text <- article_text_all[1]
  } else if(article_text == article_title){
    article_text <- article_text_all[10]
  } else{article_text <- article_text}
  df_article[nrow(df_article) + 1,] <- as.list(c(article_title, 
                                                 author, 
                                                 pub_date, 
                                                 source,
                                                 article_text,
                                                 link))
}                         


## Write df to csv
write_csv(df_article, here::here(paste0("data/processed/newdata_for_model_testing_text_", Sys.Date(), ".csv")), 
          col_names = TRUE, 
          append = TRUE)

                         
                         
                         
                         
                         
                         