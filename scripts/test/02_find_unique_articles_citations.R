library(tidyverse)
library(stringr)

pq_articles <- read_csv("data/processed/citation_29-02-2024.csv")

articles <- as.data.frame(unique(pq_articles$Title))

#---Load in the text files from ProQuest-----

data <- read_file("data/original/ProQuestDocuments-2024-03-05_02.txt")

#Separate the articles  

sep <- "____________________________________________________________"

pattern <- paste0(sep,"\\s*(.*?)\\s*",sep)

result <- regmatches(data, gregexpr(pattern, data))

df <- result %>%
  map_df(as_tibble)

number <- 1:84
doc_num <- paste0("Document ", number, " ")

pattern_2 <- paste0(doc_num,"\\s*(.*?)\\s*",sep)



new_df <- data.frame(doc_id=character(),
                     info=character())

sep <- "____________________________________________________________"
number <- 1:84
doc_num <- paste0("Document ", number, " ")

doc_id <- list()
info <- list()

for (i in doc_num){
  #print(i)
  
  doc_id <- i
  tmp_pattern <- paste0(doc_num,"\\s*(.*?)\\s*",sep)
  info <- regmatches(data, gregexpr(tmp_pattern, data))
  dat <- data.frame(x = info)
  output <- c(doc_id, info)
}

df_2 <- as.data.frame(output)
result_2 <- regmatches(data, gregexpr(pattern_2, data))

df_2 <- result_2 %>%
  map_df(as_tibble)

library(janeaustenr)

books <- austen_books()
                            