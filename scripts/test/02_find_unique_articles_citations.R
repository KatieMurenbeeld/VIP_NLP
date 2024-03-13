library(tidyverse)
library(stringr)
library(tidytext)

tdm_articles <- read_csv(here::here("data/original/citation.csv/citation.csv"))
  
#articles <- as.data.frame(unique(pq_articles$Title))

#---Load in the text files from ProQuest-----

pql_articles <- read_file(here::here("data/original/ProQuestDocuments-2024-03-12.txt"))

#Separate the articles  


sep <- "____________________________________________________________"
number <- 1:175
doc_num <- paste0("Document ", number, " ")

pattern <- paste0("\\s*(.*?)\\s*",sep)
full_text <- regmatches(pql_articles, gregexpr(tmp_pattern, pql_articles))

pql_df <- data.frame(doc_id = character(),
                     title = character(),
                     text = character())

doc_id <- list()
text <- list()
title <- list()

for (i in 1:175){
  tmp_id <- paste0("Document ", i)
  tmp_text <- full_text[[1]][i+1]
  tmp_title <- str_extract(tmp_text, regex("(?<=Title: ).*"))
  text <- str_extract_all(tmp_text, regex("(?<=Full text: ).*"))
  doc_id <- tmp_id
  text <- tmp_text
  title <- tmp_title
  pql_df[nrow(pql_df) + 1,] <- c(doc_id,
                                 title,
                                 text)
}

#articles <- new_df %>%
#  separate_rows(text, sep = "\r\n\r\n")

#write.csv(articles, file = here::here(paste0("data/processed/02_module_articles_", Sys.Date(), ".csv")), row.names = FALSE)

 
#----Compare the two datasets----

pql_df$title <- tolower(pql_df$title)
pql_df$title <- gsub("[[:punct:]]", "", pql_df$title)

pql_titles <- pql_df %>% 
  group_by(title) %>% 
  count(title) %>%
  ungroup %>%
  filter(!grepl("bless the beasts", title)) %>%
  filter(!grepl("a tough way", title))

tdm_articles$Title <- tolower(tdm_articles$Title)
tdm_articles$Title <- gsub("[[:punct:]]", "", tdm_articles$Title)

tdm_titles <- tdm_articles %>%
  group_by(Title) %>%
  count(Title) %>%
  ungroup %>%
  filter(!grepl("bless the beasts", Title)) %>%
  filter(!grepl("a tough way", Title))

titles <- full_join(pql_titles, tdm_titles, 
                    by = c("title" = "Title"),
                    suffix = c("_pql", "_tdm"))

# Load in the coded article csv
article_coding <- read_csv("data/original/new_article_coding.csv")
article_coding$Title <- trimws(tolower(article_coding$Title))
article_coding$Title <- gsub("[[:punct:]]", "", article_coding$Title)

# Create a list of unique article titles
og_titles <- trimws(unique(article_coding$Title))

og_titles <- article_coding %>%
  group_by(Title) %>%
  count(Title) %>%
  ungroup

title_compare <- full_join(titles, og_titles, 
                           by = c("title" = "Title"))

write.csv(title_compare, file = here::here(paste0("data/processed/article_library_compare_", Sys.Date(), ".csv")), row.names = FALSE)

