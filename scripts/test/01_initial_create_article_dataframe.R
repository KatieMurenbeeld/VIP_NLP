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
article_list_prev <- read.csv(file = "data/processed/article_list_20-02-2024-1405.csv") 
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

urls <- trimws(unique(grizz$Link))

# Set up a user agent so that the website doesn't think you are a robot
ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

# Create an empty date. We will append the new article to this data frame
df_article <- data.frame(Title=character(),
                         Author=character(),
                         Published_Date=character(),
                         Article_Text=character(), 
                         Link=character())

#df_article <- read_csv("data/processed/article_text.csv")

for (url in urls){
  link <- url
  page <- GET(link, user_agent(ua))
  page_text <- httr::content(page, as = "text")
  tmp <- gsub(".*<h1", "", page_text) 
  tmp_noline <- str_remove_all(tmp, "\n")
  article_title <- str_extract_all(tmp_noline, regex("(?<=clipcopy\">).+?(?=</h1>)"))
  if(lengths(article_title) == 0){
    article_title <- "none given"
  } else{article_title <- article_title}
  author <- str_extract_all(tmp_noline, regex("(?<=Byline: ).+?(?=</span>)"))
  if(lengths(author) == 0){
    author <- "none given"
  } else{author <- author}
  pub_date <- str_extract_all(tmp_noline, regex("(?<=display-date\">).+?(?=</span>)"))
  if(lengths(pub_date) == 0){
    pub_date <- "none given"
  } else{pub_date <- pub_date}
  article_text <- str_extract(tmp_noline, regex("(?<=body--ascii \">).+?(?=</p>)"))
  if(lengths(article_text) == 0){
    article_text <- "pdf of actual newspaper"
  } else{article_text <- article_text}
  df_article[nrow(df_article) + 1,] <- c(article_title, 
                                         author, 
                                         pub_date, 
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
write_csv(df_article, here::here(paste0("data/processed/article_text_", Sys.Date(), ".csv")), 
                                        col_names = TRUE, 
                                        append = TRUE)


## Join df_article to new_article_codes_id.csv
## Trim whitespace from links
article_codes$Link <- trimws(article_codes$Link)
df_text_codes <- full_join(article_codes, df_article, by="Link")

## Write joined df to csv
write_csv(df_text_codes, here::here(paste0("data/processed/article_text_codes_", 
                                           Sys.Date(), ".csv")), 
                                           col_names = TRUE, 
                                           append = TRUE)

#################
# Breadcrumb: Need to figure out if else for articles with no by lines (if character(0) then NA else text)



