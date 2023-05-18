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
# library(vscDebugger)

# FUNCTIONS
clear_junk <- function(value) {
  value <- str_remove_all(value, pattern = "u'")
  value <- str_remove_all(value, pattern = "'")
  value <- ifelse(str_to_lower(value) == "none", NA, value)
  value <- ifelse(value == "1", "low", value)
  value <- ifelse(value == "2", "medium", value)
  value <- ifelse(value == "3", "high", value)
  value <- ifelse(is.na(value), 'False', value) 
  return(value)
}

fix_values <- function(attribute) {
  lapply(X = attribute, FUN = function(t) clear_junk(t))
}

create_steps <- function(data_frame) {
  print(length(data_frame))
  m <- hashmap()
  sequence <- seq(from = 0, to = 10, length.out = length(as.list(data_frame)))
  print(length(sequence))
  m[data_frame] <- sequence
  return(m)
}

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
    attributes = attributes.y
  )
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
  mutate_at(c(5:34), fix_values) %>%
  mutate_at(c(5:34), unlist)


# How to do categorical linear regression?
# In academic_dataset_review, we have stars, useful amount, funny amount, and cool amount
# these are numerical values, but do they add any value?

# https://stats.oarc.ucla.edu/spss/faq/coding-systems-for-categorical-variables-in-regression-analysis-2/#SIMPLE%20EFFECT%20CODING
# dummy coding would work well here

# think of interesting attributes, use stars as dependent

# accepts credit cards
# take out
# noise level
# outdoor seating
# good for kids
# wifi
# alcohol types
# table service

# make overall array
overall_attributes <- c("
  accepts credit cards", "does not accept credit cards, no credit card info",
  "has takeout", "does not have takeout", "no takeout info",
  "average level noise", "quiet level noise", "very loud level noise", "no noise level information",
  "has outdoor seating", "does not have outdoor seating", "no outdoor seating info",
  "good for kids", "not good for kids", "no info on if good for kids",
)

##############################################


unique_credit_card_levels <- workable_data_2 %>%
  distinct(BusinessAcceptsCreditCards)
# True, False, NA
# 1, 0, -1



##############################################

unique_takeout_levels <- workable_data_2 %>%
  distinct(RestaurantsTakeOut)

# True, False, NA

##############################################

unique_noise_level <- workable_data_2 %>%
  distinct(NoiseLevel)

# NA, average, quiet, loud, very_loud

##############################################

unique_outdoor_seating <- workable_data_2 %>%
  distinct(OutdoorSeating)

# True, False, NA

##############################################

unique_good_for_kids <- workable_data_2 %>%
  distinct(GoodForKids)

# NA, True, False

##############################################

unique_wifi <- workable_data_2 %>%
  distinct(WiFi)

# free, NA, no, paid

##############################################

unique_alcohol_types <- workable_data_2 %>%
  distinct(Alcohol)

# NA, full_bar, beer_and_wine

##############################################

unique_table_service <- workable_data_2 %>%
  distinct(RestaurantsTableService)

# NA, False, True

##############################################


# Use 0 - 1, define count


# would it make sense to split them off then add together the winner
# for each?

View(create_steps(unique_alcohol_types$Alcohol))


credit_card_mapping <- create_steps(unique_credit_card_levels$BusinessAcceptsCreditCards)
takeout_mapping <- create_steps(unique_takeout_levels$RestaurantsTakeOut)
noise_level_mapping <- create_steps(unique_noise_level$NoiseLevel)
outdoor_seating_mapping <- create_steps(unique_outdoor_seating$OutdoorSeating)
good_for_kids_mapping <- create_steps(unique_good_for_kids$GoodForKids)
wifi_mapping <- create_steps(unique_wifi$WiFi)
alcohol_mapping <- create_steps(unique_alcohol_types$Alcohol)
table_service_mapping <- create_steps(unique_table_service$RestaurantsTableService)


# do simple regression first

##############################################
credit_card_regression <- workable_data_2 %>%
  select(stars.x, BusinessAcceptsCreditCards) %>%
  filter(!is.na(BusinessAcceptsCreditCards)) %>%
  mutate(accepts_credit_cards = as.numeric(credit_card_mapping[BusinessAcceptsCreditCards]))

print(cor(credit_card_regression$stars.x, credit_card_regression$accepts_credit_cards))
##############################################

##############################################
take_out_regression <- workable_data_2 %>%
  select(stars.x, RestaurantsTakeOut) %>%
  filter(!is.na(RestaurantsTakeOut)) %>%
  mutate(does_takeout = as.numeric(takeout_mapping[RestaurantsTakeOut]))

print(cor(take_out_regression$stars, take_out_regression$does_takeout))
##############################################

##############################################
noise_level_regression <- workable_data_2 %>%
  select(stars.x, NoiseLevel) %>%
  filter(!is.na(NoiseLevel)) %>%
  mutate(noise_level = as.numeric(noise_level_mapping[NoiseLevel]))

print(cor(noise_level_regression$stars.x, noise_level_regression$noise_level))
##############################################

##############################################
outdoor_seating_regression <- workable_data_2 %>%
  select(stars.x, OutdoorSeating) %>%
  filter(!is.na(OutdoorSeating)) %>%
  mutate(outdoor_seating = as.numeric(outdoor_seating_mapping[OutdoorSeating]))

print(cor(outdoor_seating_regression$stars.x, outdoor_seating_regression$outdoor_seating))
##############################################

##############################################
good_for_kids_regression <- workable_data_2 %>%
  select(stars.x, GoodForKids) %>%
  filter(!is.na(GoodForKids)) %>%
  mutate(good_for_kids = as.numeric(good_for_kids_mapping[GoodForKids]))

print(cor(good_for_kids_regression$stars.x, good_for_kids_regression$good_for_kids))
##############################################

##############################################
wifi_regression <- workable_data_2 %>%
  select(stars.x, WiFi) %>%
  filter(!is.na(WiFi)) %>%
  mutate(wifi_val = as.numeric(wifi_mapping[WiFi]))

print(cor(wifi_regression$stars.x, wifi_regression$wifi_val))
##############################################

##############################################
alcohol_regression <- workable_data_2 %>%
  select(stars.x, Alcohol) %>%
  filter(!is.na(Alcohol)) %>%
  mutate(alcohol_val = as.numeric(alcohol_mapping[Alcohol]))

print(cor(alcohol_regression$stars.x, alcohol_regression$alcohol_val))
##############################################

##############################################
table_service_regression <- workable_data_2 %>%
  select(stars.x, RestaurantsTableService) %>%
  filter(!is.na(RestaurantsTableService)) %>%
  mutate(has_table_service = as.numeric(table_service_mapping[RestaurantsTableService]))

print(cor(table_service_regression$stars.x, table_service_regression$has_table_service))
##############################################


# noise level, outdoor seating, and alcohol all have the highest
# need new mapping for all three of these



expanded_factors <- workable_data_2 %>%
  mutate_at(c(5:34), unlist) %>%
  mutate_at(c(5:34), as.factor)
  # mutate_at(c(5:34, na.omit)
# mutate_at(c(5:34), fix_values)


fit <- lm(expanded_factors$stars.x ~
  expanded_factors$BusinessAcceptsCreditCards +
  expanded_factors$BikeParking +
  expanded_factors$RestaurantsTakeOut +
  expanded_factors$RestaurantsDelivery +
  expanded_factors$WiFi +
  expanded_factors$WheelchairAccessible +
  expanded_factors$HappyHour +
  expanded_factors$OutdoorSeating +
  expanded_factors$HasTV +
  expanded_factors$RestaurantsReservations +
  expanded_factors$DogsAllowed +
  expanded_factors$Alcohol +
  expanded_factors$RestaurantsTableService +
  expanded_factors$RestaurantsGoodForGroups +
  expanded_factors$DriveThru +
  expanded_factors$NoiseLevel +
  expanded_factors$Smoking +
  expanded_factors$GoodForDancing +
  expanded_factors$Open24Hours
)

summary(fit)

#get counts and remove those with least amouints of datakeep like top 5 or 10

ggplot(fit, aes(x = fit$fitted.values, y = fit$residuals)) + 
  geom_point() +
  stat_smooth(method = "lm")

abline(fit)

# numerous factors end up with 2e^-16, which implies that they provide very little significance

# take the most influential factors
smaller_fit <- lm(expanded_factors$stars.x ~
  expanded_factors$WiFi +
  expanded_factors$Smoking +
  expanded_factors$Open24Hours +
  expanded_factors$GoodForDancing
)
anova(smaller_fit)
anova(fit)
summary(smaller_fit)

plot(smaller_fit)

# What are the most important factors that people care about?
# Wifi
# Smoking
# Open 24 hours
# Good for dancing

ggplot(smaller_fit, aes(x = smaller_fit$fitted.values, y = smaller_fit$residuals)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")




parse_stars <- function(star) {
  ifelse(star >= 2.5, 1, 0)
}

formatted_stars <- workable_data_2 %>%
  mutate(stars = parse_stars(stars.x)) %>%
  select(-c(business_id, name.x, review_count.x, stars.x)) %>%
  mutate_at(c(
    "ByAppointmentOnly", "BusinessAcceptsCreditCards",
    "BikeParking", "RestaurantsPriceRange2",
    "CoatCheck", "RestaurantsTakeOut",
    "RestaurantsDelivery", "Caters",
    "WiFi", "WheelchairAccessible",
    "HappyHour", "OutdoorSeating",
    "HasTV", "RestaurantsReservations",
    "DogsAllowed", "Alcohol",
    "GoodForKids","RestaurantsAttire",
    "RestaurantsTableService", "RestaurantsGoodForGroups",
    "DriveThru", "NoiseLevel",
    "Smoking", "GoodForDancing",
    "AcceptsInsurance", "BYOB",
    "Corkage", "BYOBCorkage",
    "Open24Hours", "RestaurantsCounterService"
  ), unlist) %>%
  mutate_at(c(
    "ByAppointmentOnly", "BusinessAcceptsCreditCards",
    "BikeParking", "RestaurantsPriceRange2",
    "CoatCheck", "RestaurantsTakeOut",
    "RestaurantsDelivery", "Caters",
    "WiFi", "WheelchairAccessible",
    "HappyHour", "OutdoorSeating",
    "HasTV", "RestaurantsReservations",
    "DogsAllowed", "Alcohol",
    "GoodForKids","RestaurantsAttire",
    "RestaurantsTableService", "RestaurantsGoodForGroups",
    "DriveThru", "NoiseLevel",
    "Smoking", "GoodForDancing",
    "AcceptsInsurance", "BYOB",
    "Corkage", "BYOBCorkage",
    "Open24Hours", "RestaurantsCounterService"
  ), as.factor)


logistic <- glm(stars ~ .,
  data = formatted_stars,
  family = "binomial",
)

summary(logistic)

predicted_data <- data.frame(
  prob_of_2_point_5_or_higher = logistic$fitted.values,
  two_point_or_higher = logistic$y
)

predicted_data <- predicted_data[
  order(predicted_data$prob_of_2_point_5_or_higher, decreasing = FALSE),
]

predicted_data$rank = 1:nrow(predicted_data)

ggplot(data = predicted_data, aes(x = rank, y = prob_of_2_point_5_or_higher)) +
  geom_point(aes(color = prob_of_2_point_5_or_higher))
 


write.csv(expanded_factors, "attributes", row.names=TRUE)
