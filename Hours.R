##### LOAD LIBRARIES
library(data.table)
library(tidyr)
library(tidyjson)
library(stringr)
library(dplyr)
library(purrr)
library(stringr)
library(jsonlite)
library(rjson)
library(ggplot2)

library(r2r)

# read in restaurants csv
restaurants_csv <- fread("Data/restaurants.csv")

# read in restaurants json
resturants_json <- stream_in(file("Data/yelp_academic_dataset_business.json"))

# (A left join ought to fix this problem)
joined_data <- restaurants_csv %>%
  left_join(resturants_json, by = "business_id") %>%
  mutate(
    name = name.x,
    stars = stars.x,
    review_count = review_count.x,
    is_open = is_open.x,
    hours = hours.x
  ) 
select(name, stars, review_count, is_open, hours)

View(joined_data$hours[1])

hours_data <- joined_data %>%
  pull(hours)


data_expanded <- joined_data %>%
  bind_cols(hours_data)



plz_work <- unlist(strsplit(joined_data$hours[1], split=","))
View(plz_work[1])
View(plz_work[2])

str_match(plz_work[1], "\"{'(?:\\w+)': (?:.*)")



yelpInfo <- joined_data
yelpInfo$Monday <- str_extract(yelpInfo$hours,"(?<='Monday': ')\\d{1,2}:\\d{1,2}-\\d{1,2}:\\d{1,2}(?=')")
yelpInfo$Tuesday <- str_extract(yelpInfo$hours,"(?<='Tuesday': ')\\d:\\d-\\d\\d:\\d(?=')")
yelpInfo$Wednesday <- str_extract(yelpInfo$hours,"(?<='Wednesday': ')\\d:\\d-\\d\\d:\\d(?=')")
yelpInfo$Thursday <- str_extract(yelpInfo$hours,"(?<='Thursday': ')\\d:\\d-\\d\\d:\\d(?=')")
yelpInfo$Friday <- str_extract(yelpInfo$hours,"(?<='Friday': ')\\d:\\d-\\d\\d:\\d(?=')")
yelpInfo$saturday <- str_extract(yelpInfo$hours,"(?<='Saturday': ')\\d:\\d-\\d\\d:\\d(?=')")
yelpInfo$Sunday <- str_extract(yelpInfo$hours,"(?<='Sunday': ')\\d:\\d-\\d\\d:\\d(?=')")