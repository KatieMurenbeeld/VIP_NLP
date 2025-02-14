library(tidyverse)
library(readxl)
library(RJSONIO)
library(MetBrewer)
library(googledrive)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

#---Load the data set----
gbear_meta <- read_csv(here::here("data/original/metadata_w_coverage_type_gamma_0.5.csv"))
gbear_05_preds <- read_csv(here::here("output/predictions/grizzly_bear_05_preds_gamma.csv"))

# Load the google sheet with the updated publication locations
#folder_url <- "https://drive.google.com/drive/folders/1ob5sagTtT3svhc7ZKeemd9TiAq1_MsCL"
#folder <- drive_get(as_id(folder_url))

#gdrive_files <- drive_ls(folder)
#id <- gdrive_files[gdrive_files$name == "James Edited Values", ]$id
#drive_download(id, path = "data/original/james_edited_values_coding.csv", overwrite = TRUE)

pub_loc <- read_csv(file = here::here("data/original/publication_info_gamma_0.5_2025_01_31.csv"))
pub_loc_dup <- pub_loc[!duplicated(pub_loc$`Publication ID`),]

#---Select the data and add a Country column and fill with "US"
## group by publication and year with a count of articles for year

gbear_meta$Date <- as.Date(gbear_meta$Date) 

gbear_meta <- gbear_meta %>%
  mutate(year = year(Date), 
         month = month(Date))

gbear_meta_sel <- gbear_meta %>%
  dplyr::select(GOID, `Publication Title`, `Publication ID`, `Publisher City`, `Publisher Province`, Date, year, month)

## combine with the predicted data GOID = Article ID?
gbear_preds <- gbear_05_preds %>%
  dplyr::select(Article_ID, reg_05_pred_class, knn_05_pred_class, rf_05_pred_class)

gbear_meta_pred <- left_join(gbear_preds, gbear_meta_sel, by = c("Article_ID" = "GOID"))
gbear_meta_pred <- left_join(gbear_meta_pred, pub_loc_dup, by = "Publication ID")

gbear_pred_map <- gbear_meta_pred %>% 
  select(`Publication Title.x`, reg_05_pred_class, year, pub_city, pub_state) %>%
  group_by(`Publication Title.x`, reg_05_pred_class, year) %>%
  mutate(count = n(),
         Pub_city = pub_city,
         Pub_state = pub_state)
           # Pub_city = `Publisher City`)

#pub_loc_sel <- pub_loc %>%
#  dplyr::select(`Publication ID`, `Publication Title`, `Publisher Name`, `Publisher City`, Notes)



#count_test <- gbear_meta_pred %>% 
#  group_by(`Publication Title`, reg_05_pred_class, year) %>%
#  summarise(count = n())

# Test out the open street mapping
gbear_pred_map$Country <- "US"

## create testing subsample
#get a sample of 10
set.seed(2486)
test <- sample(nrow(gbear_pred_map),20)
#data in the sample
test <- gbear_pred_map[test,]

nrow <- nrow(test)
counter <- 1
test$lon[counter] <- 0
test$lat[counter] <- 0
while (counter <= nrow){
  CityName <- gsub(' ','%20',test$Pub_city[counter]) #remove space for URLs
  CountryCode <- test$Country[counter]
  url <- paste(
    "http://nominatim.openstreetmap.org/search?city="
    , CityName
    , "&countrycodes="
    , CountryCode
    , "&limit=9&format=json"
    , sep="")
  x <- fromJSON(url)
  if(is.vector(x)){
    test$lon[counter] <- x[[1]]$lon
    test$lat[counter] <- x[[1]]$lat    
  }
  counter <- counter + 1
}

library(sf)
library(ggplot2)
library(gganimate)

my_sf <- st_as_sf(test, coords = c('lon', 'lat'), crs = "EPSG:4326")

# load tigris shapes
library(tigris)
# Download the states
states <- tigris::states(year = 2023)
states_proj <- states %>%
  st_transform("EPSG:4326")
states_proj <- states_proj %>%
  filter(as.numeric(GEOID) < 60 & as.numeric(GEOID) != 02 & as.numeric(GEOID) != 15)

#Plot it:

ggplot(my_sf) + 
  geom_sf(aes(color = reg_05_pred_class))

ggplot() + 
  geom_sf(data = states_proj, color = NA) +
  geom_sf(data = my_sf, aes(color = reg_05_pred_class, size = count)) + 
  theme_bw()


## try with full, maybe I could animate but for now pick a few "snapshots"
nrow <- nrow(gbear_pred_map)
counter <- 1
gbear_pred_map$lon[counter] <- 0
gbear_pred_map$lat[counter] <- 0
while (counter <= nrow){
  CityName <- gsub(' ','%20',gbear_pred_map$Pub_city[counter]) #remove space for URLs
  CountryCode <- gbear_pred_map$Country[counter]
  url <- paste(
    "http://nominatim.openstreetmap.org/search?city="
    , CityName
    , "&countrycodes="
    , CountryCode
    , "&limit=9&format=json"
    , sep="")
  x <- fromJSON(url)
  if(is.vector(x)){
    gbear_pred_map$lon[counter] <- x[[1]]$lon
    gbear_pred_map$lat[counter] <- x[[1]]$lat    
  }
  counter <- counter + 1
}

## save as results, the above code takes a minute to run
write_csv(gbear_pred_map, file = here::here(paste0("data/processed/gbear_preds_publisher_location_", 
                                                   Sys.Date(), ".csv")))

gbear_pred_map <- read_csv(here::here("data/processed/gbear_preds_publisher_location_2025-02-12.csv"))

## should I group by the points? 
gbear_preds_1990 <- gbear_pred_map %>%
  filter(year <= 1990)
gbear_preds_2000 <- gbear_pred_map %>%
  filter(year == 2000)
gbear_preds_2010 <- gbear_pred_map %>%
  filter(year == 2010)
gbear_preds_2020 <- gbear_pred_map %>%
  filter(year == 2020)

gbear_preds <- gbear_pred_map %>%
  filter(lon != 0)

library(sf)
library(ggplot2)

sf_1990 <- st_as_sf(gbear_preds_1990, coords = c('lon', 'lat'), crs = "EPSG:4326")
sf_2000 <- st_as_sf(gbear_preds_2000, coords = c('lon', 'lat'), crs = "EPSG:4326")
sf_2010 <- st_as_sf(gbear_preds_2010, coords = c('lon', 'lat'), crs = "EPSG:4326")
sf_2020 <- st_as_sf(gbear_preds_2020, coords = c('lon', 'lat'), crs = "EPSG:4326")

sf_all <- st_as_sf(gbear_preds, coords = c('lon', 'lat'), crs = "EPSG:4326")

# load tigris shapes
library(tigris)
# Download the states
states <- tigris::states(year = 2023)
states_proj <- states %>%
  st_transform("EPSG:4326")
states_proj <- states_proj %>%
  filter(as.numeric(GEOID) < 60 & as.numeric(GEOID) != 02 & as.numeric(GEOID) != 15)

#Plot it:
gbear_reg_1990_map <- ggplot() + 
  geom_sf(data = states_proj, color = "black") +
  geom_sf(data = sf_1990, aes(color = as.factor(reg_05_pred_class), size = count, alpha = 0.75)) + 
  scale_alpha(guide = "none") + 
  scale_size(name = "Number of Articles") +
  scale_color_met_d("Derain") +
  guides(color=guide_legend(title="Predicted Value Orientation")) +
  labs(title = "Number of Grizzly Bear Articles, 1990", 
       subtitle = "Regression Model\n(gamma threshold = 0.5)") +
  theme(legend.position = "bottom")
gbear_reg_1990_map
ggsave(here::here(paste0("output/plots/gbear_reg_mod_gt05_1990_map_", 
                         Sys.Date(), ".png")),
       gbear_reg_1990_map, height = 10, width = 12, dpi = 300)

# test an animation with years <= 1990
test_anim <- ggplot() + 
  geom_sf(data = states_proj, color = "black") +
  geom_sf(data = sf_1990, aes(color = as.factor(reg_05_pred_class), size = count, alpha = 0.75)) + 
  scale_alpha(guide = "none") + 
  scale_size(name = "Number of Articles") +
  scale_color_met_d("Derain") +
  guides(color=guide_legend(title="Predicted Value Orientation")) +
  labs(title = "Number of Grizzly Bear Articles: {frame_time}", 
       subtitle = "Regression Model\n(gamma threshold = 0.5)") + 
  theme(legend.position = "bottom") +
  transition_time(year)
animate(test_anim, nframes = 11, fps = 2)

test_anim_all <- ggplot() + 
  geom_sf(data = states_proj, color = "black") +
  geom_sf(data = sf_all, aes(color = as.factor(reg_05_pred_class), size = count, alpha = 0.75)) + 
  scale_alpha(guide = "none") + 
  scale_size(name = "Number of Articles") +
  scale_color_met_d("Derain") +
  guides(color=guide_legend(title="Predicted Value Orientation")) +
  labs(title = "Number of Grizzly Bear Articles: {frame_time}", 
       subtitle = "Regression Model\n(gamma threshold = 0.5)") + 
  theme(legend.position = "bottom") +
  transition_time(year)
animate(test_anim_all, nframes = 45, fps = 2)

anim_save(here::here("output/plots/test_reg_year_animation.gif"), animation = last_animation())

state_map <- ggplot() +
  geom_sf(data = states_proj, color = "black")

test_anim <- state_map +
  geom_sf(data = sf_1990, aes(color = as.factor(reg_05_pred_class), size = count, alpha = 0.75)) + 
  scale_alpha(guide = "none") + 
  scale_size(name = "Number of Articles") +
  scale_color_met_d("Derain") +

gbear_reg_2000_map <- ggplot() + 
  geom_sf(data = states_proj, color = "black") +
  geom_sf(data = sf_2000, aes(color = as.factor(reg_05_pred_class), size = count, alpha = 0.75)) + 
  scale_alpha(guide = "none") + 
  scale_size(name = "Number of Articles") +
  scale_color_met_d("Derain") +
  guides(color=guide_legend(title="Predicted Value Orientation")) +
  labs(title = "Number of Grizzly Bear Articles, 2000", 
       subtitle = "Regression Model\n(gamma threshold = 0.5)") +
  theme(legend.position = "bottom")
gbear_reg_2000_map
ggsave(here::here(paste0("output/plots/gbear_reg_mod_gt05_2000_map_", 
                         Sys.Date(), ".png")),
       gbear_reg_2000_map, height = 10, width = 12, dpi = 300)

gbear_reg_2010_map <- ggplot() + 
  geom_sf(data = states_proj, color = "black") +
  geom_sf(data = sf_2010, aes(color = as.factor(reg_05_pred_class), size = count, alpha = 0.75)) + 
  scale_alpha(guide = "none") + 
  scale_size(name = "Number of Articles") +
  scale_color_met_d("Derain") +
  guides(color=guide_legend(title="Predicted Value Orientation")) +
  labs(title = "Number of Grizzly Bear Articles, 2010", 
       subtitle = "Regression Model\n(gamma threshold = 0.5)") +
  theme(legend.position = "bottom")
gbear_reg_2010_map
ggsave(here::here(paste0("output/plots/gbear_reg_mod_gt05_2010_map_", 
                         Sys.Date(), ".png")),
       gbear_reg_2010_map, height = 10, width = 12, dpi = 300)

gbear_reg_2020_map <- ggplot() + 
  geom_sf(data = states_proj, color = "black") +
  geom_sf(data = sf_2020, aes(color = as.factor(reg_05_pred_class), size = count, alpha = 0.75)) + 
  scale_alpha(guide = "none") + 
  scale_size(name = "Number of Articles") +
  scale_color_met_d("Derain") +
  guides(color=guide_legend(title="Predicted Value Orientation")) +
  labs(title = "Number of Grizzly Bear Articles, 2020", 
       subtitle = "Regression Model\n(gamma threshold = 0.5)") +
  theme(legend.position = "bottom")
gbear_reg_2020_map
ggsave(here::here(paste0("output/plots/gbear_reg_mod_gt05_2020_map_", 
                         Sys.Date(), ".png")),
       gbear_reg_2020_map, height = 10, width = 12, dpi = 300)
