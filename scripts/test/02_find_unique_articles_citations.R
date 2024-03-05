library(tidyverse)
library(stringr)

pq_articles <- read_csv("data/processed/citation_29-02-2024.csv")

articles <- as.data.frame(unique(pq_articles$Title))

#---Load in the text files from ProQuest-----

data <- read_file("data/original/ProQuestDocuments-2024-03-05.txt")

sep <- "____________________________________________________________"

pattern <- paste0(sep,"\\s*(.*?)\\s*",sep)

result <- regmatches(data, gregexpr(pattern, data))

df <- result %>%
  map_df(as_tibble)


                            