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
article_codes <- read.csv(file = "data/processed/new_article_coding_ids.csv")
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
urls <- trimws(unique(article_list_prev$Link))

# Set up a user agent so that the website doesn't think you are a robot
ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

# Create an empty date. We will append the new article to this data frame
df_article <- data.frame(Title=character(),
                         Author=character(),
                         Published_Date=character(),
                         Article_Text=character(), 
                         Link=character())

df_article <- read_csv("data/processed/article_text.csv")

for (url in urls[1:50]){
  link <- url
  page <- GET(link, user_agent(ua))
  page_text <- httr::content(page, as = "text")
  tmp <- gsub(".*<h1", "", page_text) 
  article_title <- str_extract_all(tmp, regex("(?<=clipcopy\">).+?(?=</h1>)"))
  author <- str_extract_all(tmp, regex("(?<=Byline: ).+?(?=</span>)"))
  if(lengths(author) == 0){
    author <- "none given"
  } else{author <- author}
  pub_date <- str_extract_all(tmp, regex("(?<=display-date\">).+?(?=</span>)"))
  tmp_noline <- str_remove_all(tmp, "\n")
  article_text <- str_extract(tmp_noline, regex("(?<=<p>).+?(?=</p>)"))
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
write_csv(df_article, "data/processed/article_text.csv", col_names = TRUE, append = TRUE)


## Join df_article to new_article_codes_id.csv
## Trim whitespace from links
article_codes$Link <- trimws(article_codes$Link)
df_text_codes <- full_join(article_codes, df_article, by="Link")

## Write joined df to csv
write_csv(df_text_codes, "data/processed/article_codes_text.csv", )

#################
# Breadcrumb: Need to figure out if else for articles with no by lines (if character(0) then NA else text)



