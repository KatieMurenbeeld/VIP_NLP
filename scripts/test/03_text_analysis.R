library(stringr)
library(tidyverse)
library(ggplot2)
library(janeaustenr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()


# read in the data

original_articles <- read_csv(here::here("data/processed/02_module_articles_2024-03-06.csv"))
