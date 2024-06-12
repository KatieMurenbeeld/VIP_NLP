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
#article_list_new <- article_codes %>%
#  select(X, Link, ID)

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

species <- "grizzly_bears"
urls <- trimws(unique(beavs$Link))

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

for (url in urls){
  link <- url
  page <- GET(link, user_agent(ua))
  page_text <- read_html(page) 
  article_title <- page_text %>% html_element("h1") %>% html_text2()
  article_text_all <- page_text %>% html_elements("p") %>% html_text2()
  if(length(article_title) == 0){
    article_title <- "none given"
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
  } else{article_text <- article_text}
  if(article_text == article_title){
    article_text <- article_text_all[10]
  } else{article_text <- article_text}
  df_article[nrow(df_article) + 1,] <- c(article_title, 
                                         author, 
                                         pub_date, 
                                         source,
                                         article_text,
                                         link)
}


## Write df to csv
write_csv(df_article, here::here(paste0("data/processed/article_text_", species, Sys.Date(), ".csv")), 
                                        col_names = TRUE, 
                                        append = TRUE)


## Join df_article to new_article_codes_id.csv
## Trim whitespace from links
article_codes$Link <- trimws(article_codes$Link)
df_text_codes <- left_join(df_article, article_codes, by="Link")

## Write joined df to csv
write_csv(df_text_codes, here::here(paste0("data/processed/article_text_codes_beavers_", 
                                           Sys.Date(), ".csv")), 
                                           col_names = TRUE, 
                                           append = TRUE)


### testing, not all articles have the same number of "p" elements
link <- urls[6]
page <- GET(link, user_agent(ua))
page_text <- read_html(page) 
page_text %>% html_element("h1") %>% html_text2()
page_text %>% html_nodes("span.author-byline") %>% html_text2()
page_text %>% html_elements("p") %>% html_text2()
