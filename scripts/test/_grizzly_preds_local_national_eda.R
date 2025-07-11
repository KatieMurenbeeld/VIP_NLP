library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(MetBrewer)


# Load the data
#----

## Load the grizzly corpus predictions
gbear_05_preds <- read_csv(here::here("output/predictions/grizzly_bear_05_preds_gamma.csv"))
#gbear_055_preds <- read_csv(here::here("output/predictions/grizzly_bear_055_preds_gamma.csv"))
#gbear_06_preds <- read_csv(here::here("output/predictions/grizzly_bear_06_preds_gamma.csv"))
#gbear_065_preds <- read_csv(here::here("output/predictions/grizzly_bear_065_preds_gamma.csv"))

#gbear_05_preds <- read_csv(here::here("output/predictions/grizzly_bear_05_preds_gamma_2025-01-31.csv"))

## Load the meta data (this is on our SPASES google drive)
gbear_meta <- read_csv(here::here("data/original/metadata_w_coverage_type_gamma_0.5.csv"))

### join the prediction and meta data
#gbear_05 <- left_join(gbear_meta, gbear_05_preds, by = c("GOID" = "Article_ID"))
gbear_05 <- left_join(gbear_05_preds, gbear_meta, by = c("Article_ID" = "GOID"))

## Load the newspaper location data (this is on our SPASES google drive)
pub_loc <- read_csv(file = here::here("data/original/publication_info_gamma_0.5_2025_01_31.csv"))
pub_loc_dup <- pub_loc[!duplicated(pub_loc$`Publication ID`),] # remove duplicates

### Join predition+meta data and the location data
gbear_05_full <- left_join(gbear_05, pub_loc_dup, by = "Publication ID")

## Load the training data (not sure I actually use this)
training <- read_csv(here::here("data/processed/clean_text_2024-09-16.csv"))

# for the training data drop any rows with NA for Value_Orientation
training <- training[!is.na(training$Value_Orientation), ]
train_df <- training %>%
  mutate(type = "training") %>%
  dplyr::select(type, Value_Orientation) %>%
  rename("value" = "Value_Orientation")

# Start looking at the predicted classification through time
#----

gbear_05_time <- gbear_05_full %>% # select relevant variables
  dplyr::select(Date.x, `Publication Title.x`, `Coverage Type.x`, pub_city, pub_state, reg_05_pred_class, knn_05_pred_class, rf_05_pred_class) %>%
  mutate(gamma_thres = "gamma thres = 0.5")


gbear_05_time$Date.x <- as.Date(gbear_05_time$Date.x) # make the date datetime

gbear_05_time <- gbear_05_time %>% # add columns for the month, year, and month-year
  mutate(year = year(Date.x), 
         month = month(Date.x),
         month_yr = format(as.Date(Date.x), "%Y-%m"))

gbear_05_time$month_yr <- as.Date(paste(gbear_05_time$month_yr, "-01", sep="")) # this needs to be in yr-mn-day for some reason so I just paste on a -01

gbear_05_time %>% # group by the year and coverage type and get the number of article by coverage type per year
  group_by(year, `Coverage Type.x`) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count, color = `Coverage Type.x`)) + 
  geom_line()

gbear_05_time %>% # group by the month and coverage type and get the number of article by coverage type per month
  #filter(year > 2020) %>%
  group_by(month_yr, `Coverage Type.x`) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = as.Date(month_yr), y = count, color = `Coverage Type.x`)) + 
  geom_line() 

gbear_nat <- gbear_05_time %>% # create a national subset
  filter(`Coverage Type.x` == "national")

gbear_grange <- gbear_05_time %>% # create a local and in grizzly range subset
  filter(`Coverage Type.x` == "local") %>%
  filter(pub_state %in% c("WA", "WY", "ID", "MT", "CO", "OR", "CA"))

gbear_sub <- rbind(gbear_nat, gbear_grange) # combine the national and local+range subsets

## make some quick plots
gbear_sub %>% 
  group_by(month_yr, `Coverage Type.x`) %>% # group by the month and coverage type and get the number of article by coverage type per month
  summarise(count = n()) %>%
  ggplot(., aes(x = as.Date(month_yr), y = count, color = `Coverage Type.x`)) + 
  geom_line()

gbear_sub %>% 
  group_by(year, `Coverage Type.x`) %>% # group by the year and coverage type and get the number of article by coverage type per year
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count, color = `Coverage Type.x`)) + 
  geom_line()

gbear_grange %>% 
  group_by(year, pub_state) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count, color = pub_state)) + 
  geom_line()

gbear_sub %>% 
  group_by(year, reg_05_pred_class, `Coverage Type.x`) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count, alpha = as.factor(reg_05_pred_class))) + 
  geom_line(aes(color = as.factor(reg_05_pred_class)))+ 
  scale_color_met_d("Derain")+
  scale_alpha_manual(values = c("1" = 1.0, "2" = 1.0, "3" = 0.3, "4" = 0.3, "5" = 0.3, "6" = 1.0, "7" = 1.0)) +
  labs(title = "Number of Grizzly Bear Articles", 
       subtitle = "Reg Model\n(gamma threshold = 0.5)",
       x = "Year", y = "Count", 
       fill = "Predicted\nValue Orientation") +
  facet_wrap(~`Coverage Type.x`)

year_type_pred <- gbear_sub %>% # group by year, WVO, and coverage type - get the number of articles of class per year
  group_by(year, reg_05_pred_class, `Coverage Type.x`) %>%
  summarise(count = n())

test <- gbear_sub %>% # try to get the coverage type as a percentage by year
  add_count(year, `Coverage Type.x`) %>% 
  add_count(year, name = 'year_n') %>% 
  group_by(year, `Coverage Type.x`) %>%
  summarise(count_percentage = first(n)/first(year_n)) %>% 
  mutate(count_percentage = scales::percent(count_percentage))

test_2 <- gbear_sub %>% # try to get the number of mutualism (WVO = 6, 7) as a percentage by year
  filter(`Coverage Type.x` == "national") %>%
  add_count(year, reg_05_pred_class) %>% 
  add_count(year, name = 'year_n') %>% 
  group_by(year, reg_05_pred_class) %>%
  summarise(dom_percentage = ifelse(reg_05_pred_class >= 6,
                                     first(n)/first(year_n), 
                                     0)) %>% 
  mutate(dom_percentage = scales::percent(dom_percentage))

test_3 <- gbear_sub %>% # not sure what I was trying to do here
  filter(`Coverage Type.x` == "local") %>%
  add_count(year, reg_05_pred_class) %>% 
  add_count(year, name = 'year_n') %>% 
  group_by(year, reg_05_pred_class) %>%
  summarise(dom_percentage = ifelse(reg_05_pred_class >= 6,
                                    first(n)/first(year_n), 
                                    0)) %>% 
  mutate(dom_percentage = scales::percent(dom_percentage))

total_preds <- gbear_sub %>% 
  group_by(year, `Coverage Type.x`) %>%
  summarise(count = n())

gbear_sub %>% 
  filter(`Coverage Type.x` == "national") %>%
  group_by(year, `Coverage Type.x`, reg_05_pred_class) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count, alpha = as.factor(reg_05_pred_class))) + 
  geom_line(aes(color = as.factor(reg_05_pred_class)))+ 
  scale_color_met_d("Derain")+
  scale_alpha_manual(values = c("1" = 1.0, "2" = 1.0, "3" = 0.3, "4" = 0.3, "5" = 0.3, "6" = 1.0, "7" = 1.0)) +
  labs(title = "Number of Grizzly Bear Articles", 
       subtitle = "Reg Model\n(gamma threshold = 0.5)",
       x = "Year", y = "Count", 
       fill = "Predicted\nValue Orientation")

gbear_05_time %>% 
  group_by(year, reg_05_pred_class) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = year, y = count, alpha = as.factor(reg_05_pred_class))) + 
  geom_line(aes(color = as.factor(reg_05_pred_class)))+ 
  scale_color_met_d("Derain")+
  scale_alpha_manual(values = c("1" = 1.0, "2" = 1.0, "3" = 0.3, "4" = 0.3, "5" = 0.3, "6" = 1.0, "7" = 1.0)) +
  labs(title = "Number of Grizzly Bear Articles", 
       subtitle = "Reg Model\n(gamma threshold = 0.5)",
       x = "Year", y = "Count", 
       fill = "Predicted\nValue Orientation") 

## Here I try to get the % of mutual, neutral, domination articles by year
## on the full dataset and the "sub" dataset with only the national+local in range articles

gbear_05_year_pct <- gbear_05_time %>% 
  group_by(year, reg_05_pred_class) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = reg_05_pred_class, values_from = count, values_fill = 0) %>%
  mutate(pct_mut = (`1` + `2`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_neu = (`3` + `4` + `5`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_dom = (`6` + `7`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100)

gbear_sub_year_pct <- gbear_sub %>% 
  group_by(year, reg_05_pred_class) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = reg_05_pred_class, values_from = count, values_fill = 0) %>%
  mutate(pct_mut = (`1` + `2`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_neu = (`3` + `4` + `5`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_dom = (`6` + `7`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100)

## Here I try to weight the reclassification of the WVO rankings
gbear_sub_year_pct_test <- gbear_sub %>% 
  group_by(year, reg_05_pred_class, `Coverage Type.x`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = reg_05_pred_class, values_from = count, values_fill = 0) %>%
  mutate(pct_mut = (`1` + `2` + `3`/2) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_neu = (`3`/2 + `4` + `5`/2) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_dom = (`5`/2 + `6` + `7`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100)

# Here I calculate the % of each predicted class by year for use in a heat map
gbear_sub_year_pct_heat <- gbear_sub %>% 
  group_by(year, reg_05_pred_class, `Coverage Type.x`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = reg_05_pred_class, values_from = count, values_fill = 0) %>%
  mutate(pct_1 = (`1`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_2 = (`2`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_3 = (`3`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_4 = (`4`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_5 = (`5`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_6 = (`6`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100) %>%
  mutate(pct_7 = (`7`) / (`1` + `2` + `3` + `4` + `5` + `6` + `7`) * 100)

gbear_05_time %>% # reshape the data
  group_by(year, reg_05_pred_class) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = reg_05_pred_class, values_from = count, values_fill = 0)

# year over year growth or % of articles through time
gbear_05_time %>% 
  group_by(year, `Coverage Type.x`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = `Coverage Type.x`, values_from = count, values_fill = 0) %>%
  mutate(pct_national = national / (national + local + regional) * 100) %>%
  ggplot(., aes(x = year, y = pct_national)) + 
  geom_line()
 # geom_line(aes(color = as.factor(reg_05_pred_class)))

# simple plots of % mutualism, neutral, or domination articles
## full dataset
gbear_05_year_pct %>%
  ggplot(., aes(x = year, y = pct_mut)) +
  geom_line()

gbear_05_year_pct %>%
  ggplot(., aes(x = year, y = pct_neu)) +
  geom_line()

gbear_05_year_pct %>%
  ggplot(., aes(x = year, y = pct_dom)) +
  geom_line()

## subset of data: national+local in range
gbear_sub_year_pct %>%
  ggplot(., aes(x = year, y = pct_mut)) +
  geom_line()

gbear_sub_year_pct %>%
  ggplot(., aes(x = year, y = pct_neu)) +
  geom_line()

gbear_sub_year_pct %>%
  ggplot(., aes(x = year, y = pct_dom)) +
  geom_line()

# plots with different segments of time
## subset of data: national+local in range
gbear_sub_year_pct_test %>%
  filter(year >= 2000) %>%
  ggplot(., aes(x = year, y = pct_mut)) +
  geom_line() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~`Coverage Type.x`)

gbear_sub_year_pct_test %>%
  filter(year >= 2000) %>%
  ggplot(., aes(x = year, y = pct_neu)) +
  geom_line() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~`Coverage Type.x`)

gbear_sub_year_pct_test %>%
  filter(year >= 2000) %>%
  ggplot(., aes(x = year, y = pct_dom)) +
  geom_line() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~`Coverage Type.x`)

gbear_sub_year_pct_test %>%
  filter(year >= 2000) %>%
  ggplot(., aes(x = year, y = pct_dom)) +
  geom_line() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~`Coverage Type.x`)

gbear_sub_year_pct_test %>%
  ggplot(., aes(year)) + 
  geom_line(aes(y = pct_mut, color = "% mutual")) + 
  geom_line(aes(y = pct_neu, color = "% neutral")) + 
  geom_line(aes(y = pct_dom, color = "% dominant")) + 
  geom_smooth(aes(y = pct_mut), method = "lm") +
  geom_smooth(aes(y = pct_neu), method = "lm") +
  geom_smooth(aes(y = pct_dom), method = "lm") +
  facet_wrap(~`Coverage Type.x`)

gbear_sub_year_pct_test %>%
  filter(year >= 1990) %>%
  ggplot(., aes(year)) + 
  geom_line(aes(y = pct_mut, color = "% mutual")) + 
  geom_line(aes(y = pct_neu, color = "% neutral")) + 
  geom_line(aes(y = pct_dom, color = "% dominant")) + 
  geom_smooth(aes(y = pct_mut), method = "lm") +
  geom_smooth(aes(y = pct_neu), method = "lm") +
  geom_smooth(aes(y = pct_dom), method = "lm") +
  facet_wrap(~`Coverage Type.x`)

gbear_sub_year_pct_test %>%
  filter(year >= 2000) %>%
  ggplot(., aes(year)) + 
  geom_line(aes(y = pct_mut, color = "% mutual")) + 
  geom_line(aes(y = pct_neu, color = "% neutral")) + 
  geom_line(aes(y = pct_dom, color = "% dominant")) + 
  geom_smooth(aes(y = pct_mut), method = "lm") +
  geom_smooth(aes(y = pct_neu), method = "lm") +
  geom_smooth(aes(y = pct_dom), method = "lm") +
  facet_wrap(~`Coverage Type.x`)

gbear_sub_year_pct_test %>%
  filter(year >= 1990) %>%
  filter(year <= 2017) %>%
  ggplot(., aes(year)) + 
  geom_line(aes(y = pct_mut, color = "% mutual")) + 
  geom_line(aes(y = pct_neu, color = "% neutral")) + 
  geom_line(aes(y = pct_dom, color = "% dominant")) + 
  geom_smooth(aes(y = pct_mut), method = "lm") +
  geom_smooth(aes(y = pct_neu), method = "lm") +
  geom_smooth(aes(y = pct_dom), method = "lm") +
  facet_wrap(~`Coverage Type.x`)


# for heat map
## have to reshape the data
heat_test <- gbear_sub_year_pct_heat %>%
  select(`Coverage Type.x`, year, 10:16) %>%
  pivot_longer(
    cols = pct_1:pct_7, 
    names_to = "wvo_pred",
    values_to = "pct"
  ) %>%
  arrange(year)

## testing a heat map
## depth of colors in the grid correspond to the % of articles that year with the predicted WVO 
heat_test %>%
  filter(year >= 1990) %>%
  ggplot(., aes(year, wvo_pred, fill= pct)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="salmon") +
  facet_wrap(~`Coverage Type.x`) + 
  theme_bw()


