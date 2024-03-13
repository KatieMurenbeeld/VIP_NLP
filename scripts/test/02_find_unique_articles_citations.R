library(tidyverse)
library(stringr)

tdm_articles <- read_csv(here::here("data/original/citation.csv/citation.csv"))
  
#articles <- as.data.frame(unique(pq_articles$Title))

#---Load in the text files from ProQuest-----

pql_articles <- read_file(here::here("data/original/ProQuestDocuments-2024-03-12.txt"))

#Separate the articles  
pql_df <- data.frame(doc_id = character(),
                     title = character(),
                     text = character())

sep <- "____________________________________________________________"
number <- 1:84
doc_num <- paste0("Document ", number, " ")

doc_id <- list()
text <- list()
title <- list()

pattern <- paste0("\\s*(.*?)\\s*",sep)
tmp_text <- regmatches(pql_articles, gregexpr(tmp_pattern, pql_articles))

for (i in doc_num){
  tmp_id <- i
  tmp_pattern <- paste0("\\s*(.*?)\\s*",sep)
  tmp_text <- regmatches(pql_articles, gregexpr(tmp_pattern, pql_articles))
  tmp_title <- str_extract(tmp_text, regex("(?<=Title:).*"))
  doc_id <- tmp_id
  text <- tmp_text
  pql_df[nrow(pql_df) + 1,] <- c(doc_id,
                                 title,
                                 text)
}

articles <- new_df %>%
  separate_rows(text, sep = "\r\n\r\n")

write.csv(articles, file = here::here(paste0("data/processed/02_module_articles_", Sys.Date(), ".csv")), row.names = FALSE)

                            