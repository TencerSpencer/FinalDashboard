##### LOAD LIBRARIES
library(data.table)
library(dplyr)
library(tidyr)
library(tidyjson)
library(jsonlite)
library(stringr)
#library(stringr)
#library(broom)
#library(geojsonio)
#library(leaflet)
#library(htmlwidgets)
#library(webshot)
#ibrary(mapview)
#library(httr)
#library(sf)
#library(sp)


library(dplyr)
library(purrr)
library(stringr)
library(jsonlite)
library(tidyr)
library(rjson)

# What packages can be leveraged with shiny to make life easier?
# https://www.r-bloggers.com/2017/07/the-r-shiny-packages-you-need-for-your-web-apps/
## https://ebailey78.github.io/shinyBS/index.html (tool tips)
## https://github.com/Yang-Tang/shinyjqui (animation and user customization)
## https://dreamrs.github.io/shinyWidgets/ (fancier styled buttons etc)
## https://rstudio.github.io/DT/ (really intuitive table usage)


# https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
# https://www.scrapingbee.com/blog/web-scraping-r/

# LOAD DATA

# Note that a place's name can be figured out by the URL it redirects to.. we can take the top 10 or so of this? Maybe more
# review data
#review_dataset <- fread("Data/yelp_academic_dataset_review.csv", nrows = 5000)
  
# business data
#buisness_dataset <- fread("Data/yelp_academic_dataset_business.csv", nrows = 5000)

# checkin data
#checkin_dataset <- fread("Data/yelp_academic_dataset_checkin.csv", nrows = 5000)


# check most 5 stars for a cat
# sorted_top_ten <- review_dataset %>%
#     select(business_id, stars) %>%
#     group_by(business_id) %>%
#     mutate(stars = as.numeric(stars)) %>%
#     summarise(sum(stars)) %>%
#     top_n(30)


# # check whom reviews the most places
# # this is currently incorrect
# sorted_top_ten_users <- review_dataset %>%
#     select(user_id, business_id) %>%
#     group_by(user_id) %>%
#     distinct(business_id) %>%
#     top_n(30)




# What about attributes? Can I access all of the unique ones?
# first row in double, non single quotes
# test <- gsub("'", '"', restaurants$attributes[1])
# 
# remove double quotes that repeat
# test2 <- gsub('"+', '"', test)

# remove any '\'?
# test3 <-  gsub('\\\\', "", test2)

# test <- restaurants  %>% 
#   mutate(test = str_replace_all(attributes,
#       "\\(([^,]+),\\s*([^)]+)\\)", "\\2:\\1") %>% 
#    str_replace(fixed("["), "[{") %>%
#    str_replace(fixed("]"), "}]") %>%
#    str_replace_all(fixed("'"), '') %>%
#    str_replace_all(fixed("\""), '') %>% 
#    map(jsonlite::fromJSON)) %>%
#    unnest(attributes) %>%
#  type.convert(as.is = TRUE)

 # parse error: after key and value, inside map, I expect ',' or '}'
 # this is more helpful



# another test
# test12 <- test %>% str(max.level = 1)
# test45 <- "\"attributes\": {\"RestaurantsDelivery\": \"False\", \"OutdoorSeating\": \"False\", \"BusinessAcceptsCreditCards\": \"False\", \"BusinessParking\": \"\"\"\"{\"garage\": False, \"street\": True, \"validated\": False, \"lot\": False, \"valet\": False}\"\"\"\", \"BikeParking\": \"True\", \"RestaurantsPriceRange2\": \"1\", \"RestaurantsTakeOut\": \"True\", \"ByAppointmentOnly\": \"False\", \"WiFi\": \"\"\"\"u\"free\"\"\"\"\", \"Alcohol\": \"\"\"\"u\"none\"\"\"\"\", \"Caters\": \"True\"}"

# test46 <- jsonlite::parse_json(test45)
# test46 <- rjson::fromJSON(test45)
# View(test46)



# read in restaurants csv
restaurants_csv <- fread("Data/restaurants.csv", nrow = 5000)

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


# what attribute fields are nested?
# businessParking, music, best nights, good for, ambience

# average rating
# identify attributes we want to investigate.. i.e. ByAppointmentOnly (how many true vs false)
# BusinessAcceptsCreditCards, Alcohol, (Happyhour?), (smoking?) (business accepts bit coin?)
# Drivethru, DogsAllowed

clear_junk <- function(value) {
  value <- str_remove_all(value, pattern='u\'')
  value <- str_remove_all(value, pattern = '\'')
  value <- ifelse(str_to_lower(value) == "none", NA, value)
  return(value)
}

fix_values <- function(attribute) {
  lapply(X = attribute, FUN = function(t) clear_junk(t))
}

#test <- joined_data %>%
#  mutate(
#    by_appointment_only = fix_values(attributes$ByAppointmentOnly),
#    business_accepts_credit_cards = fix_values(attributes$BusinessAcceptsCreditCards),
#    alcohol = fix_values(attributes$Alcohol),
#    happy_hour = fix_values(attributes$HappyHour),
#    smoking = fix_values(attributes$Smoking),
#    business_accepts_bit_coins = fix_values(attributes$BusinessAcceptsBitcoin),
#    drive_through = fix_values(attributes$DriveThru),
#    dogs_allowed = fix_values(attributes$DogsAllowed),
#    restaurant_reservations = fix_values(attributes$RestaurantsReservations)
#  )
#
#test2 <- joined_data %>%
#  pull(attributes) %>%
#  purrr::map_df(~ purrr::map_chr(.x ~ fix_values(.x))) %>%
#  bind_cols(joined_data %>%
#    select(business_id), .)
#
# dont do factor conversions until we test a specific area
# there are permutations for alcohol that need to be cleaned up
# smoking also needs to be adjusted, swap things to logicals that are good


# is it more beneficial for a place to offer alcohol or to allow for reservations?


# of these these fields, what provides the highest average amount of stars?

# extract attributes from the attributes field
attributes_data <- joined_data %>%
  pull(attributes)

# piece together the attributes and the business together
data_expanded <- joined_data %>%
  bind_cols(attributes_data)

# filter for only wanted rows, ignore compounded columns
# compounded columns are: 43 (BusinessParking), 53 (Ambience), 
# 58 (GoodForMeal), 61 (Music), 64 (Best Nights), these can be evaluated later
workable_data <- data_expanded %>%
  select(c(2, 3, 11, 10, 34:42, 44:52, 54:57, 59, 60, 62, 63, 65:72))

# Not all columns look great, we need to take those that have extra noise and clean them
workable_data_2 <- workable_data %>%
  mutate_at(c(5:38), fix_values)


alcohol_average_rating <- workable_data_2 %>%
  group_by(Alcohol) %>%
  summarise(average_rating = mean(stars.x))


# do I want to melt this
melted_data <- melt(workable_data_2, id.vars = c("business_id", "name.x", "review_count.x", "stars.x"))

highest_average_rated_attribute <- melted_data %>%
  mutate(variable_pair = paste(variable, value, sep=": ")) %>%
  group_by(variable_pair) %>%
  summarise(average_rating = mean(stars.x)) %>%
  top_n(10)


ggplot(highest_average_rated_attribute, aes(average_rating, variable_pair))





# dietary restrictions is pretty interesting, give this one more attention, abandon computations above





