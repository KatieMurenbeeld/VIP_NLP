library(word2vec)
library(tidyverse)
library(tidytext)
library(slider)
library(purrr)
library(widyr)
library(furrr)
library(ggplot2)
library(RColorBrewer)
library(viridis)

# load the parameter values
wv_df <- read_csv(here::here("output/word_vector_param_2024-10-17.csv"))
wv_ns_df <- read_csv(here::here("output/word_vector_param_n_sample_2024-10-31.csv"))

# Here we tested accuracy by seeing how well the models will predict x
# a:b::y:x
# a - b + y = x
# pred["grizzly", ] - pred["bear", ] + pred["feral", ] 
# x should = "hog"

wv_for_plots <- wv_df %>%
  filter(term == "hog") %>%
  group_by(type, dim, window) %>%
  summarise(ave_sim = similarity)

wv_ns_for_plots_cbow <- wv_ns_df %>%
  filter(term == "hog") %>%
  filter(type == "cbow") %>%
  group_by(dim, window, n_samples) %>%
  summarise(ave_sim = similarity)

wv_ns_for_plots_sg <- wv_ns_df %>%
  filter(term == "hog") %>%
  filter(type == "skip-gram") %>%
  group_by(dim, window, n_samples) %>%
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

# might need to do heat maps, still facet wrap by dim but have n_samples on one axis
# and window on another and fill the boxes with similarity 
test_params_ns_cbow <- ggplot(wv_ns_for_plots_cbow) + 
  geom_raster(aes(x = window, y = n_samples, fill = ave_sim)) + 
  geom_text(aes(x = window, y = n_samples, label = round(ave_sim, 2)), size = 2.5)+
  facet_wrap(~dim,
             labeller = label_both) +
  scale_fill_viridis() +
  coord_fixed(ratio=2) +
  ylab(label = "Negative Samples") +
  xlab(label = "Window") +
  labs(title = "Predicting 'hog'", 
       subtitle = "negative sampling: cbow") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

test_params_ns_cbow
ggsave(here::here("output/word_embedding_param_select_ns_cbow.png"), plot = test_params_ns_cbow,
       width = 6, height = 4, dpi = 300)

test_params_ns_sg <- ggplot(wv_ns_for_plots_sg) + 
  geom_raster(aes(x = window, y = n_samples, fill = ave_sim)) + 
  geom_text(aes(x = window, y = n_samples, label = round(ave_sim, 2)), size = 2.5)+
  facet_wrap(~dim,
             labeller = label_both) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.75) +
  ylab(label = "Negative Samples") +
  xlab(label = "Window") +
  labs(title = "Predicting 'hog'", 
       subtitle = "negative sampling: skip-gram") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")
test_params_ns_sg
ggsave(here::here("output/word_embedding_param_select_ns_skip.png"), plot = test_params_ns_sg,
       width = 6, height = 4, dpi = 300)

