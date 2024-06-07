library(httr)
library(tidyverse)
library(stringr)
library(rvest)
library(googledrive)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Load the new_article_coding_ids.csv
article_codes <- read.csv(file = "data/original/new_article_coding.csv")
article_list_prev <- read.csv(file = "data/processed/article_list_07-06-2024-1430.csv") 
article_list_prev <- article_list_prev %>% 
  select(X, Link, ID)

## Get new urls from article_codes
article_list_new <- article_codes %>%
  select(X, Link, ID)

# Create list of article urls
## need to trim the leading and trailing white spaces
#article_links <- anti_join(article_list_new, article_list_prev, by="ID")
#urls <- trimws(unique(article_links$Link))
grizz <- article_codes %>%
  filter(Species == "Grizzly Bear" | Species == "Grizzly Bears")

beavs <- article_codes %>%
  filter(Species == "Beaver" | Species == "Beavers")

wolf <- article_codes %>%
  filter(Species == "Wolves")

boars <- article_codes %>%
  filter(Species == "Boars")

other <- article_codes %>%
  filter(Species == "Other")

urls <- trimws(unique(grizz$Link))

# Set up a user agent so that the website doesn't think you are a robot
ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

# Create an empty date. We will append the new article to this data frame
df_article <- data.frame(Title = character(),
                         Author = character(),
                         Published_Date = character(),
                         Source = character(),
                         Article_Text = character(), 
                         Link = character())

#df_article <- read_csv("data/processed/article_text.csv")

for (url in urls[1:2]){
  link <- url
  page <- GET(link, user_agent(ua))
  page_text <- read_html(page) 
  article_title <- page_text %>% html_element("h1") %>% html_text2()
  if(lengths(article_title) == 0){
    article_title <- "none given"
  } else{article_title <- article_title}
  author <- page_text %>% html_nodes("span.author-byline") %>% html_text2()
  if(lengths(author) == 0){
    author <- "none given"
  } else{author <- author}
  pub_date <- page_text %>% html_nodes("span.display-date") %>% html_text2()
  if(lengths(pub_date) == 0){
    pub_date <- "none given"
  } else{pub_date <- pub_date}
  source <- page_text %>% html_nodes("span.source") %>% html_text2()
  if(lengths(source) == 0){
    source <- "none given"
  } else{source <- source}
  article_text_all <- page_text %>% html_elements("p") %>% html_text2()
  article_text <- article_text_all[9]
  if(lengths(article_text) == 0){
    article_text <- "pdf of actual newspaper"
  } else{article_text <- article_text}
  df_article_test[nrow(df_article_test) + 1,] <- c(article_title, 
                                                   author, 
                                                   pub_date, 
                                                   source,
                                                   article_text,
                                                   link)
}

## testing using read_html() from rvest
page2 <- GET(urls[2], user_agent(ua))
page2_test <- read_html(page2)
author_text2 <- page2_test %>% html_nodes("span.author-byline") %>% html_text2()
date_text2 <- page2_test %>% html_nodes("span.display-date") %>% html_text2()
source_text2 <- page2_test %>% html_nodes("span.source") %>% html_text2()
text2_test <- page2_test %>% html_elements("p") %>% html_text2()
article_text2 <- text2_test[9]
title_text2 <- page2_test %>% html_element("h1") %>% html_text2()

## test new for-loop
### create new data frame that includes source
df_article_test <- data.frame(Title = character(),
                              Author = character(),
                              Published_Date = character(),
                              Source = character(),
                              Article_Text = character(), 
                              Link = character())

for (url in urls[1:2]){
  link <- url
  page <- GET(link, user_agent(ua))
  page_text <- read_html(page) 
  article_title <- page_text %>% html_element("h1") %>% html_text2()
  if(lengths(article_title) == 0){
    article_title <- "none given"
  } else{article_title <- article_title}
  author <- page_text %>% html_nodes("span.author-byline") %>% html_text2()
  if(lengths(author) == 0){
    author <- "none given"
  } else{author <- author}
  pub_date <- page_text %>% html_nodes("span.display-date") %>% html_text2()
  if(lengths(pub_date) == 0){
    pub_date <- "none given"
  } else{pub_date <- pub_date}
  source <- page_text %>% html_nodes("span.source") %>% html_text2()
  if(lengths(source) == 0){
    source <- "none given"
  } else{source <- source}
  article_text_all <- page_text %>% html_elements("p") %>% html_text2()
  article_text <- article_text_all[9]
  if(lengths(article_text) == 0){
    article_text <- "pdf of actual newspaper"
  } else{article_text <- article_text}
  df_article_test[nrow(df_article_test) + 1,] <- c(article_title, 
                                                   author, 
                                                   pub_date, 
                                                   source,
                                                   article_text,
                                                   link)
}



## Not sure if this is necessary or if I can just join by the link
#df_article$ID <- df_article %>% 
#  filter(!Link=='') %>%
#  mutate(tmp = match(Link, unique(Link)),
#         ID = sprintf('Article%003d', tmp)) %>%
#  select(-tmp)

## Write df to csv
write_csv(df_article, here::here(paste0("data/processed/article_text_coyote_", Sys.Date(), ".csv")), 
                                        col_names = TRUE, 
                                        append = TRUE)


## Join df_article to new_article_codes_id.csv
## Trim whitespace from links
article_codes$Link <- trimws(article_codes$Link)
df_text_codes <- full_join(article_codes, df_article, by="Link")

## Write joined df to csv
write_csv(df_text_codes, here::here(paste0("data/processed/article_text_codes_coyote_", 
                                           Sys.Date(), ".csv")), 
                                           col_names = TRUE, 
                                           append = TRUE)

#################
# Breadcrumb: Need to figure out if else for articles with no by lines (if character(0) then NA else text)



