##### LOAD LIBRARIES
library(data.table)
library(dplyr)
library(tidyr)
library(tidyjson)
library(jsonlite)
library(stringr)
library(dplyr)
library(purrr)
library(stringr)
library(jsonlite)
library(tidyr)
library(rjson)

# FUNCTIONS
clear_junk <- function(value) {
  value <- str_remove_all(value, pattern='u\'')
  value <- str_remove_all(value, pattern = '\'')
  value <- ifelse(str_to_lower(value) == "none", NA, value)
  return(value)
}

fix_values <- function(attribute) {
  lapply(X = attribute, FUN = function(t) clear_junk(t))
}


# read in restaurants csv
restaurants_csv <- fread("Data/restaurants.csv")

# read in restaurants json
resturants_json <- stream_in(file("Data/yelp_academic_dataset_business.json"))

# note that the csv will have less records than the json. We will be using the json for now.


# This join needs to be re-evaluated, dont keep non-restaurant records
joined_data <- restaurants_csv %>%
  inner_join(resturants_json, by = 'business_id') %>%
  mutate(
    name = name.x,
    stars = stars.x,
    review_count = review_count.x,
    is_open = is_open.x,
    attributes = attributes.y)
  select(name, stars, review_count, is_open, attributes)

View(joined_data$attributes)

test <- joined_data %>%
    mutate(
        name = name.x,
        city = city.x,
        state = state.x,
        stars = stars.x,
        review_count = review_count.x,
        dietary_restrictions = attributes$DietaryRestrictions,
    ) %>%
    select(name, city, state, stars, review_count, dietary_restrictions) %>%
    filter(!is.na(dietary_restrictions))
