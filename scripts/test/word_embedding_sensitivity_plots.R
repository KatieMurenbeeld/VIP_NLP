library(word2vec)
library(tidyverse)
library(tidytext)
library(slider)
library(purrr)
library(widyr)
library(furrr)
library(ggplot2)

# load the parameter values
wv_df <- read_csv(here::here("output/word_vector_param_2024-10-17.csv"))

# Here we tested accuracy by seeing how well the models will predict x
# a:b::y:x
# a - b + y = x
# pred["grizzly", ] - pred["bear", ] + pred["feral", ] 
# x should = "hog"

wv_for_plots <- wv_df %>%
  filter(term == "hog") %>%
  group_by(type, dim, window) %>%
  summarise(ave_sim = similarity) 


test_params <- ggplot(wv_for_plots, aes(window, ave_sim)) +
  geom_line() +
  facet_wrap(~dim,
             labeller = label_both) +
  ylab(label = "Similarity") +
  labs(title = "Predicting 'hog'", 
       subtitle = "skip-gram") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")
  
test_params
ggsave(here::here("output/word_embedding_param_select_test.png"), plot = test_params,
       width = 6, height = 4, dpi = 300)
