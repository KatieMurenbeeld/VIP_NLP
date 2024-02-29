library(tidyverse)
library(stringr)

pq_articles <- read_csv("data/processed/citation_29-02-2024.csv")

articles <- as.data.frame(unique(pq_articles$Title))
