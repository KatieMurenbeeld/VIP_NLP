library(tidyverse)
library(stringr)

pq_articles <- read_csv("data/processed/citation_29-02-2024.csv")

articles <- as.data.frame(unique(pq_articles$Title))

#---Load in the text files from ProQuest-----

data <- read_file("data/original/ProQuestDocuments-2024-03-05_02.txt")

#Separate the articles  
new_df <- data.frame(doc_id=character(),
                     info=character())

sep <- "____________________________________________________________"
number <- 1:84
doc_num <- paste0("Document ", number, " ")

doc_id <- list()
info <- list()

for (i in doc_num){
  tmp_id <- i
  tmp_pattern <- paste0(i,"\\s*(.*?)\\s*",sep)
  tmp_info <- regmatches(data, gregexpr(tmp_pattern, data))
  doc_id <- tmp_id
  info <- tmp_info
  new_df[nrow(new_df) + 1,] <- c(doc_id, 
                                 info)
}

articles <- new_df %>%
  separate_rows(info, sep = "\r\n\r\n")

write.csv(articles, file = here::here("data/processed/02_module_articles.csv"))

library(janeaustenr)

books <- austen_books()
                            