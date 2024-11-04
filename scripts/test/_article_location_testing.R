library(tidyverse)

# Load the cleaned data
article_codes <- read.csv(file = "data/original/james_edited_values_coding.csv")

# Select the link, publishing city, and publishing state

article_locations <- article_codes %>%
  select(Link, Publication_City, Publication_State, Newspaper) %>%
  distinct(., Link, .keep_all = TRUE)

test <- article_locations %>%
  filter(Publication_State == "ID") %>%
  filter(Publication_City != "")


# the following code is from 
# https://stackoverflow.com/questions/13905098/how-to-get-the-longitude-and-latitude-coordinates-from-a-city-name-and-country-i

library(RJSONIO)
nrow <- nrow(test)
counter <- 1
test$lon[counter] <- 0
test$lat[counter] <- 0
while (counter <= nrow){
  CityName <- gsub(' ','%20',test$Publication_City[counter]) #remove space for URLs
  StateCode <- test$Publication_State[counter]
  url <- paste(
    "http://nominatim.openstreetmap.org/search?city="
    , CityName
    , "&statecode="
    , StateCode
    , "&limit=9&format=json"
    , sep="")
  x <- fromJSON(url)
  if(is.vector(x)){
    test$lon[counter] <- x[[1]]$lon
    test$lat[counter] <- x[[1]]$lat    
  }
  counter <- counter + 1
}

test$distribution <- 0

library(rvest)
webpage <- read_html("https://muckrack.com/rankings/top-30-idaho-newspapers")
table_node <- html_nodes(webpage, "table")
table_content <- html_table(table_node)

head(table_content)

id_distributions <- as.data.frame(table_content)
id_distributions <- id_distributions %>% separate(Location, c('City', 'State'))

write_csv(id_distributions, here::here(paste0("data/processed/id_newspaper_distributions_muckrack_", Sys.Date(), ".csv")))

