library(tidyverse)
library(stringr)

pq_articles <- read_csv("data/processed/citation_29-02-2024.csv")

articles <- as.data.frame(unique(pq_articles$Title))

#---Load in the text files from ProQuest-----

data <- read_file("data/original/ProQuestDocuments-2024-03-05_02.txt")

#Separate the articles  
new_df <- data.frame(doc_id=character(),
                     text=character())

sep <- "____________________________________________________________"
number <- 1:84
doc_num <- paste0("Document ", number, " ")

doc_id <- list()
text <- list()

for (i in doc_num){
  tmp_id <- i
  tmp_pattern <- paste0(i,"\\s*(.*?)\\s*",sep)
  tmp_text <- regmatches(data, gregexpr(tmp_pattern, data))
  doc_id <- tmp_id
  text <- tmp_text
  new_df[nrow(new_df) + 1,] <- c(doc_id, 
                                 text)
}

articles <- new_df %>%
  separate_rows(text, sep = "\r\n\r\n")

write.csv(articles, file = here::here(paste0("data/processed/02_module_articles_", Sys.Date(), ".csv")), row.names = FALSE)

                            