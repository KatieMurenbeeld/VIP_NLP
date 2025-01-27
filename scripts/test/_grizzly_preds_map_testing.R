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

## should I group by the points? 
gbear_preds_1990 <- gbear_pred_map %>%
  filter(year == 1990)
gbear_preds_2000 <- gbear_pred_map %>%
  filter(year == 2000)
gbear_preds_2010 <- gbear_pred_map %>%
  filter(year == 2010)
gbear_preds_2020 <- gbear_pred_map %>%
  filter(year == 2020)



