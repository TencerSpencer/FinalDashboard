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

attributes <- fread("Data/attributes.csv")

big_data <- fread("Data/restaurantDataVisProjData.csv")

clear_junk <- function(value) {
  value <- ifelse(str_to_lower(value) == "false", NA, value)
  value <- ifelse(value == "4", "very high", value)
  return(value)
}

price_range_business_id <- attributes %>%
    select(business_id, RestaurantsPriceRange2) %>%
    mutate(PriceRange = clear_junk(RestaurantsPriceRange2))






price_range_business_id_unique <- attributes %>%
    select(business_id, RestaurantsPriceRange2) %>%
    mutate(PriceRange = clear_junk(RestaurantsPriceRange2)) %>%
    select(business_id, PriceRange)
    distinct(business_id) 


combined_data <- big_data %>% 
    left_join(price_range_business_id, join_by('business_id'))

write.csv(combined_data, "resturantDataVisProjDataWithPriceRange.csv", row.names=TRUE)


test <- combined_data %>%
    group_by(is_chain) %>%
    count()

graph_me_daddy <- combined_data %>%
    filter(!is.na(PriceRange)) %>%
    filter(!is.na(is_chain))
# Does a higher price range indicate a higher rating?

# ggplot(credit, aes(x = Balance, y = Cards)) +
    # geom_jitter(color = "#1f1fd6") +
    # theme(
        # axis.text.x = element_text(size=18, angle=-45,face="bold"),
        # axis.text.y = element_text(size=18, face="bold"),
        # axis.title.x = element_text(size=18, face="bold"),
        # axis.title.y = element_text(size=18, face="bold"),
        # strip.text.x = element_text(size=18, face="bold"),
        # panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank()
    # ) +
    # labs(
        # x = "Balance ($)",
        # y = "Card Count"
    # )


ggplot(graph_me_daddy, aes(x = PriceRange, y = average_stars)) +
    geom_jitter()

ggplot(graph_me_daddy, aes(x = PriceRange, y = average_stars)) +
    geom_violin() +
    facet_wrap(as.factor(graph_me_daddy$is_chain))
