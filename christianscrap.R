### SUNBURST
library(dplyr)
library(tidyverse)
library(treemap)
library(sunburstR)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
finalData <- read.csv("Data/restaurantDataVisProjData.csv")
stylesData <- read.csv("Data/Food_Origins.csv")

# Reformat data for the sunburstR package
stylesData <- stylesData %>% 
  rename_with(.cols = 1, ~"business_id")

test <- finalData %>%
  left_join(stylesData, join_by('business_id'))

finalData <- test %>%
  select(business_id, name, address, latitude, longitude, states, stars, average_stars, review_count,
         total_review_count, count, is_chain, Style, Region, RestaurantsReservations, RestaurantsDelivery, 
         HappyHour, RestaurantsTakeOut, OutdoorSeating, BusinessAcceptsCreditCards, DriveThru, Open24Hours, 
         DogsAllowed, Alcohol, RestaurantsTableService, RestaurantsGoodForGroups, Monday, Tuesday, Wednesday, 
         Thursday, Friday, Saturday, Sunday)

yelpData <- finalData %>%
  filter(Style != 'Undefined' 
         & Region != 'Bar' 
         & Region != 'Coffee/Juice/Tea' 
         & Style != 'Alcohol' 
         & Style != 'NonAlcoholicDrinks') %>%
  mutate(stars = floor(stars)) %>%
  mutate(path = paste(stars, Region, Style, Style, sep="-")) %>%
  dplyr::select(path, stars)

y <- sunburst(yelpData, legend=TRUE)
y