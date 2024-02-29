library(tidyverse)
library(stringr)

# Load in the coded article csv
article_coding <- read_csv("data/original/new_article_coding.csv")

# Create a list of unique article titles
titles <- trimws(unique(article_coding$Title))

# I want a search string that looks like...
# TITLE("article title") OR TITLE("article title") OR ...

tmp_string <- paste0('TITLE("',titles,'")', sep = "")
search_string <- paste(tmp_string, collapse = " OR ")
cat(search_string)
