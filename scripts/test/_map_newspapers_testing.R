library(tidyverse)
library(readxl)
library(RJSONIO)

# Load the TDM newspaper titles data
us_w <- read_excel(here::here("data/original/tdm_database_titles/us_west_newsstream.xls"), skip = 1)
us_se <- read_excel(here::here("data/original/tdm_database_titles/us_southeast_newsstream.xls"), skip = 1) 
us_mw <- read_excel(here::here("data/original/tdm_database_titles/us_midwest_newsstream.xls"), skip = 1)
us_nc <- read_excel(here::here("data/original/tdm_database_titles/us_northcentral_newsstream.xls"), skip = 1)
us_sc <- read_excel(here::here("data/original/tdm_database_titles/us_southcentral_newsstream.xls"), skip = 1)
us_ne <- read_excel(here::here("data/original/tdm_database_titles/us_northeast_newsstream.xls"), skip = 1)
us_md <- read_excel(here::here("data/original/tdm_database_titles/us_majordailies.xls"), skip = 1)

# Combine all and remove duplicates
full_news <- bind_rows(us_w, us_se, us_mw, us_nc)

# Filter the datasets for Pub Type = Newspapers and Publication Country = United States

us_w_filt <- us_w %>%
  filter(`Pub Type` == "Newspapers",
         `Publication Country` == "United States")

filter_select <- function(x) { 
  x %>%       
    filter(`Pub Type` == "Newspapers",
           `Publication Country` == "United States") %>%
    select(Title, Publisher, `Pub Type`, `Publication Place`, `Pub ID` )
    #mutate(newsstream = as.character(x))
}

news_list <- list(us_w, us_se, us_mw, us_nc, us_sc, us_ne, us_md) %>%
  lapply(filter_select)

full_news <- do.call(rbind, news_list)

# split the publication place into two new columns, city and state
#full_news[c("city", "state")] <- do.call(rbind, strsplit(full_news$`Publication Place`, ","))
full_news <- separate(full_news, col = `Publication Place`, into = c("City", "State"), sep = ",", remove = FALSE)

# remove duplicated publications
full_news_uniq <- full_news[!duplicated(full_news), ]

# save this dataframe
write_csv(full_news_uniq, here::here(paste0("data/processed/tdm_newspapers_", Sys.Date(), ".csv")))

test <- full_news_uniq[1:10, ]
#test$State <- gsub('Calif.', "California", test$State)
#test$State <- gsub('Or.', "Oregon", test$State)
test$City <- gsub('San Bernadino', "San Bernardino", test$City)
test$Country <- "US"

nrow <- nrow(test)
counter <- 1
test$lon[counter] <- 0
test$lat[counter] <- 0
while (counter <= nrow){
  CityName <- gsub(' ','%20',test$City[counter]) #remove space for URLs
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

# Download the county data for Idaho. Set the year to 2023
states <- tigris::states(year = 2023)
states_proj <- states %>%
  st_transform("EPSG:4326")
states_proj <- states_proj %>%
  filter(GEOID < 60)
test_states <- states %>%
  filter(STUSPS %in% c("CA", "OR", "WA"))
  #dplyr::select(STUSPS, geometry)
test_states_proj <- test_states %>%
  st_transform("EPSG:4326")
my_sf_proj <- my_sf %>%
  st_transform("EPSG:4326")

#Plot it:

ggplot(my_sf_proj) + 
  geom_sf(aes(color = State))

ggplot() + 
  geom_sf(data = test_states_proj, color = NA) +
  geom_sf(data = my_sf_proj) + 
  theme_bw()
  
full_news_uniq$City <- gsub('San Bernadino', "San Bernardino", full_news_uniq$City)
full_news_uniq$City <- gsub('Ravenna-Kent', "Ravenna", full_news_uniq$City)
full_news_uniq$Country <- "US"
full_news_uniq <- full_news_uniq %>%
  filter(Title != "Pacific Daily News") # in Gaum

nrow <- nrow(full_news_uniq)
counter <- 1
full_news_uniq$lon[counter] <- 0
full_news_uniq$lat[counter] <- 0
while (counter <= nrow){
  CityName <- gsub(' ','%20',full_news_uniq$City[counter]) #remove space for URLs
  CountryCode <- full_news_uniq$Country[counter]
  url <- paste(
    "http://nominatim.openstreetmap.org/search?city="
    , CityName
    , "&countrycodes="
    , CountryCode
    , "&limit=9&format=json"
    , sep="")
  x <- fromJSON(url)
  if(is.vector(x)){
    full_news_uniq$lon[counter] <- x[[1]]$lon
    full_news_uniq$lat[counter] <- x[[1]]$lat    
  }
  counter <- counter + 1
}

full_news_sf <- st_as_sf(full_news_uniq, coords = c('lon', 'lat'), crs = "EPSG:4326")

full_news_sf_proj <- full_news_sf %>%
  st_transform("EPSG:4326")

box = c(xmin = -179.23109, ymin = 18.86546, xmax = -66, ymax = 71.43979)
states_proj_crop <- st_crop(states_proj, box)

ggplot(full_news_sf_proj) + 
  geom_sf()

ggplot() + 
  geom_sf(data = states_proj_crop, color = NA) +
  geom_sf(data = full_news_sf_proj) + 
  theme_bw()
