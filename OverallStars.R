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


# FUNCTIONS
clear_junk <- function(value) {
  value <- str_remove_all(value, pattern='u\'')
  value <- str_remove_all(value, pattern = '\'')
  value <- ifelse(str_to_lower(value) == "none", NA, value)
  value <- ifelse(value == "2", "medium", value)
  value <- ifelse(value == "3", "high", value)
  return(value)
}

fix_values <- function(attribute) {
  lapply(X = attribute, FUN = function(t) clear_junk(t))
}

# read in restaurants csv
restaurants_csv <- fread("Data/restaurants.csv")

# read in restaurants json
resturants_json <- stream_in(file("Data/yelp_academic_dataset_business.json"))

# (A left join ought to fix this problem)
joined_data <- restaurants_csv %>%
  left_join(resturants_json, by = 'business_id') %>%
  mutate(
    name = name.x,
    stars = stars.x,
    review_count = review_count.x,
    is_open = is_open.x,
    attributes = attributes.y)
  select(name, stars, review_count, is_open, attributes)

View(joined_data$attributes)


# extract attributes from the attributes field
attributes_data <- joined_data %>%
  pull(attributes)

# piece together the attributes and the business together
data_expanded <- joined_data %>%
  bind_cols(attributes_data)

# basic rows to keep are 2, 3, 11, and 10 (might have changed)
# filter for only wanted rows, ignore compounded columns
# compounded columns are: 43 (Business Parking), 53(Ambience),
# 58 (Good For Meal), 61 (Music), 64 (Best Nights), 68 (HairSpecializesIn)
# 71 (DietaryRestrictions)
workable_data <- data_expanded %>%
  select(c(2, 3, 11, 10, 34:42, 44:52, 54:57, 60, 62, 63, 65, 66, 67, 69, 70))

# sanitize values so that their permutations are sound
workable_data_2 <- workable_data %>%
  mutate_at(c(5:34), fix_values)


# melt the data for grouping
melted_data <- melt(workable_data_2, id.vars = c("business_id", "name.x", "review_count.x", "stars.x"))

# this could be optimized but im being lazy

# compute the average rating of each attribute
highest_average_rated_attribute <- melted_data %>%
  mutate(variable_pair = paste(variable, value, sep=": ")) %>%
  filter(!is.na(value)) %>%
  group_by(variable_pair) %>%
  summarise(average_rating = mean(stars.x, na.rm=TRUE))

# count the amount of each score
highest_average_rated_attribute_counts <- melted_data %>%
  mutate(variable_pair = paste(variable, value, sep=": ")) %>%
  select(variable_pair, variable, value) %>%
  group_by(variable_pair) %>%
  filter(!is.na(value)) %>%
  count()

# perform a join and remove records that are too low
highest_average_rated_attributes_finalized <- highest_average_rated_attribute %>%
  left_join(highest_average_rated_attribute_counts, by = c('variable_pair')) %>%
  filter(n >= 800) %>%
  top_n(10)


# fix any names that are strange
highest_average_rated_attributes_finalized[highest_average_rated_attributes_finalized == "RestaurantsPriceRange2"] <- "RestaurantsPriceRange"
# filter anything that has below 800 as its sum

ggplot(highest_average_rated_attributes_finalized, aes(x = average_rating, y = variable_pair)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(x = "Average Rating", y = "Attribute") +
  theme(
    axis.text.x = element_text(size=18, face="bold"),
    axis.text.y = element_text(size=18, face="bold"),
    axis.title.x = element_text(size=18, face="bold"),
    axis.title.y = element_text(size=18, face="bold"),
    strip.text.x = element_text(size=18, face="bold"),
    legend.text = element_text(size=18, face="bold"),
    legend.title = element_text(size=18,face="bold"),
  )


# Future plan: Using dashboard elements, we would include a slider, zoom, and search feature for various attributes. This would allow users to
# hone in on a given attribute. Additionally, we could also branch beyond average rating and look into if any attributes would make a user more likely
# to write up a review or not. We would have investigated dietary restrictions, though sadly there around ~30 entries out of the large dataset we have
# so we cannnot derive any meaning.



# How to do categorical linear regression?
# In academic_dataset_review, we have stars, useful amount, funny amount, and cool amount
# these are numerical values, but do they add any value?

# https://stats.oarc.ucla.edu/spss/faq/coding-systems-for-categorical-variables-in-regression-analysis-2/#SIMPLE%20EFFECT%20CODING
# dummy coding would work well here


# think of interesting attributes

# accepts credit cards
# take out
# noise level
# outdoor seating
# good for kids
# wifi
# alcohhol types 
# table service

# use only true and falses for logistical regression?
# still wouldnt work
cols_of_interest <- workable_data_2 %>%
  select(
    business_id,
    BusinessAcceptsCreditCards,
    RestaurantsTakeOut,
    NoiseLevel,
    OutdoorSeating,
    GoodForKids,
    WiFi,
    Alcohol,
    RestaurantsTableService, 
  )

# review count, stars, predicting if its open or not?
# doesnt add value, though  