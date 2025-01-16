library(tidyverse)
library(readxl)
library(RJSONIO)

#---Load the data set----
gbear_meta <- read_csv(here::here("data/original/metadata_w_coverage_type_gamma_0.5.csv"))
gbear_05_preds <- read_csv(here::here("output/predictions/grizzly_bear_05_preds_gamma.csv"))

#---Select the data and add a Country column and fill with "US"
## group by publication and year with a count of articles for year

gbear_meta$Date <- as.Date(gbear_meta$Date) 

gbear_meta <- gbear_meta %>%
  mutate(year = year(Date), 
         month = month(Date))

gbear_meta_sel <- gbear_meta %>%
  dplyr::select(GOID, `Publication Title`, `Publisher City`, `Publisher Province`, Date, year, month)

## combine with the predicted data GOID = Article ID?
gbear_preds <- gbear_05_preds %>%
  dplyr::select(Article_ID, reg_05_pred_class, knn_05_pred_class, rf_05_pred_class)

gbear_meta_pred <- left_join(gbear_preds, gbear_meta_sel, by = c("Article_ID" = "GOID"))

gbear_pred_map <- gbear_meta_pred %>% 
  select(`Publication Title`, reg_05_pred_class, year, `Publisher City`) %>%
  group_by(`Publication Title`, reg_05_pred_class, year) %>%
  mutate(count = n(),
         Pub_city = `Publisher City`)
           # Pub_city = `Publisher City`)

count_test <- gbear_meta_pred %>% 
  group_by(`Publication Title`, reg_05_pred_class, year) %>%
  summarise(count = n())

# remove duplicate?
test <- distinct(gbear_pred_map)


test <- left_join(gbear_pred_map, gbear_meta_sel)


gbear_pred_map$Country <- "US"
