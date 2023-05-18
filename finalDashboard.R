##### LOAD LIBRARIES
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(geojsonio)
library(leaflet)
library(htmlwidgets)
library(webshot)
library(mapview)
library(httr)
library(sf)
library(shiny)
library(sp)

baltimoreAddressData <- fread("Data/baltimoreAddressData.csv")

#region LOAD ALL GEOJSON DATA FOR OUR INCOME PERCENTAGE RANGES
urlLessThan25k <- parse_url("https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services")
urlLessThan25k$path <- paste(urlLessThan25k$path, "Hh25inc/FeatureServer/0/query", sep = "/")
urlLessThan25k$query <- list(where = "1=1",
                             outFields = "*",
                             returnGeometry = "true",
                             f = "geojson")
requestLessThan25k <- build_url(urlLessThan25k)

# * $25,000 to $40,000 https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Hh40inc/FeatureServer/0
url25to40k <- parse_url("https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services")
url25to40k$path <- paste(url25to40k$path, "Hh40inc/FeatureServer/0/query", sep = "/")
url25to40k$query <- list(where = "1=1",
                         outFields = "*",
                         returnGeometry = "true",
                         f = "geojson")
request25to40k <- build_url(url25to40k)

# * $40,000 to $60,000 https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Hh60inc/FeatureServer/0
url40to60k <- parse_url("https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services")
url40to60k$path <- paste(url40to60k$path, "Hh60inc/FeatureServer/0/query", sep = "/")
url40to60k$query <- list(where = "1=1",
                         outFields = "*",
                         returnGeometry = "true",
                         f = "geojson")
request40to60k <- build_url(url40to60k)

# * $60,000 to $75,000 https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Hh75inc/FeatureServer/0
url60to75k <- parse_url("https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services")
url60to75k$path <- paste(url60to75k$path, "Hh75inc/FeatureServer/0/query", sep = "/")
url60to75k$query <- list(where = "1=1",
                         outFields = "*",
                         returnGeometry = "true",
                         f = "geojson")
request60to75k <- build_url(url60to75k)

# * More than $75,000 https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Hhm75/FeatureServer/0
urlMoreThan75k <- parse_url("https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services")
urlMoreThan75k$path <- paste(urlMoreThan75k$path, "Hhm75/FeatureServer/0/query", sep = "/")
urlMoreThan75k$query <- list(where = "1=1",
                             outFields = "*",
                             returnGeometry = "true",
                             f = "geojson")
requestMoreThan75k <- build_url(urlMoreThan75k)

geoJsonLessThan25k <- st_read(requestLessThan25k)
geoJson25to40k <- st_read(request25to40k)
geoJson40to60k <- st_read(request40to60k)
geoJson60to75k <- st_read(request60to75k)
geoJsonMoreThan75k <- st_read(requestMoreThan75k)

#endregion

#region PRODUCE INFORMATION TO PARSE HOUSE SALE AVERAGE

# Its free real estate
soldAddressses <- baltimoreAddressData[baltimoreAddressData$Event %like% "PROPERTY SOLD FOR", ]
soldAddressses$SALEPRIC <- as.numeric(gsub("[^0-9]", "", soldAddressses$Event))

countyHouseAverage <- soldAddressses %>%
  group_by(County) %>%
  mutate(AVG_SALES = mean(SALEPRIC)) %>%
  select(County, AVG_SALES) %>%
  summarise_each(funs(mean)) %>%
  arrange(AVG_SALES)

# We just want the geometry, we could pull it from anywhere 
formattedCountyHouseAvg <- geoJsonLessThan25k %>%
  mutate(County = toupper(CSA2010)) %>%
  left_join(countyHouseAverage) %>%
  select(County, AVG_SALES, geometry) %>%
  mutate(County = tolower(County)) %>%
  na.omit() %>% # min/max logic breaks with NAs. Defaults might work.
  st_sf()

#endregion

#region PRODUCE INFORMATION TO PARSE MOST COMMON EVENTS

top_5_events_per <- baltimoreAddressData %>%
  filter(!startsWith(Event, "PROPERTY SOLD FOR")) %>%
  group_by(County, Event) %>%
  summarize(Event_Amount = n()) %>%
  arrange(desc(Event_Amount), .by_group = TRUE) %>%
  slice_head(n = 5)

unique_counties <- top_5_events_per %>%
  distinct(County)

event1_values <- top_5_events_per %>%
  slice(n = 1) %>%
  ungroup()

# Take each county and spread out its events into 10 new rows
top_5_events_df <- data.frame((
  top_5_events_per %>%
    slice(n = 1) %>%
    ungroup() %>%
    mutate(
      County = tolower(County),
      Event = tolower(Event)
    ) %>%
    select(County, Event, Event_Amount) %>%
    rename(
      Event1 = Event,
      Amount1 = Event_Amount
    )
), (top_5_events_per %>%
      slice(n = 2) %>%
      ungroup() %>%
      mutate(Event = tolower(Event)) %>%
      select(Event, Event_Amount) %>%
      rename(
        Event2 = Event,
        Amount2 = Event_Amount
      )
), (top_5_events_per %>%
      slice(n = 3) %>%
      ungroup() %>%
      mutate(Event = tolower(Event)) %>%
      select(Event, Event_Amount) %>%
      rename(
        Event3 = Event,
        Amount3 = Event_Amount
      )
), (top_5_events_per %>%
      slice(n = 4) %>%
      ungroup() %>%
      mutate(Event = tolower(Event)) %>%
      select(Event, Event_Amount) %>%
      rename(
        Event4 = Event,
        Amount4 = Event_Amount
      )
), (top_5_events_per %>%
      slice(n = 5) %>%
      ungroup() %>%
      mutate(Event = tolower(Event)) %>%
      select(Event, Event_Amount) %>%
      rename(
        Event5 = Event,
        Amount5 = Event_Amount
      )
)
)

## Align and merge event counts to geojson ##
eventInCounty <- baltimoreAddressData %>%
  select(Event, County) %>%
  group_by(County) %>%
  summarize(event_count = n()) %>%
  mutate(County = tolower(County))

# might want to remove more columns besides CSA
county_data_st <- geoJsonLessThan25k %>%
  mutate(County = tolower(CSA2010)) %>%
  select(-c(CSA2010)) %>%
  left_join(eventInCounty) %>%
  left_join(top_5_events_df) %>%
  st_sf()

format_tool_tip_event <- function(event, count) {
  paste(event, ": ", count, "<br>", sep = "")
}

get_event_rate_polygon <- function(map_data, labels, qal) {
  addPolygons(map = map_data, stroke = TRUE, opacity = 1, fillOpacity = 1, smoothFactor = 0.5,
              color="black",
              fillColor = ~qal(event_count),
              weight = 1, 
              label = labels,
              highlight = highlightOptions(
                weight = 3,
                stroke = 4,
                bringToFront = TRUE
              ),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
    addLegend(values=~event_count, pal=qal, title = "Event Rate", opacity = 0.7)
}

#endregion

#region VARIABLES AND FUNCTIONS FOR HOUSE SALE AVERAGE MAP

min_house_sold_price <- ceiling(min(formattedCountyHouseAvg$AVG_SALES))
max_house_sold_price <- ceiling(max(formattedCountyHouseAvg$AVG_SALES))

getAvgPropertySaleDatasetFormatted <- function(data, new_bounds) {
  
  min <- new_bounds[1]
  max <- new_bounds[2]
  
  return(data %>% filter(AVG_SALES <= max & AVG_SALES >= min))
}

get_avg_prop_sale_polygon <- function(map_data, labels, qal) {
  addPolygons(map = map_data, stroke = TRUE, opacity = 1, fillOpacity = 1, smoothFactor = 0.5,
              color="black",
              fillColor = ~qal(AVG_SALES),
              weight = 1, 
              label = labels,
              highlight = highlightOptions(
                weight = 3,
                stroke = 4,
                bringToFront = TRUE
              ),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
    addLegend(values=~AVG_SALES, pal=qal, title = "Home Price", opacity = 1)
}

#endregion

#region VARIABLES AND FUNCTIONS FOR MOST COMMON EVENTS

get_event_rate_polygon <- function(map_data, labels, qal) {
  addPolygons(map = map_data, stroke = TRUE, opacity = 1, fillOpacity = 1, smoothFactor = 0.5,
              color="black",
              fillColor = ~qal(event_count),
              weight = 1, 
              label = labels,
              highlight = highlightOptions(
                weight = 3,
                stroke = 4,
                bringToFront = TRUE
              ),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
    addLegend(values=~event_count, pal=qal, title = "Event Count", opacity = 1)
}
#endregion

#region VARIABLES AND FUNCTIONS FOR INCOME MAP

income_levels <- c("< 25k", "25k - 40k", "40k - 60k", "60k - 75k", "> 75k")

# return appropriate dataset based on selection
getIncomeDatasetFormatted <- function(desired_set) {
  
  # load desired dataset
  data <- NULL
  if (desired_set == "< 25k") {
    data <- geoJsonLessThan25k
  } else if (desired_set ==  "25k - 40k") {
    data <- geoJson25to40k
  } else if (desired_set == "40k - 60k") {
    data <- geoJson40to60k
  } else if (desired_set == "60k - 75k") {
    data <- geoJson60to75k
  } else if (desired_set == "> 75k") {
    data <- geoJsonMoreThan75k
  }
  
  # adjust naming for counties then drop the field
  formatted_data <- data %>%
    mutate(county = tolower(CSA2020)) %>%
    select(-c(CSA2010))
  
  # append income statistic to data and rename to common variable
  data_with_income <- NULL
  if (desired_set == "< 25k") {
    data_with_income <- formatted_data %>%
      select(county, hh25inc20, geometry) %>%
      rename(income2020 = hh25inc20)
  } else if (desired_set ==  "25k - 40k") {
    data_with_income <- formatted_data %>%
      select(county, hh40inc20, geometry) %>%
      rename(income2020 = hh40inc20)
  } else if (desired_set == "40k - 60k") {
    data_with_income <- formatted_data %>%
      select(county, hh60inc20, geometry) %>%
      rename(income2020 = hh60inc20)
  } else if (desired_set == "60k - 75k") {
    data_with_income <- formatted_data %>%
      select(county, hh75inc20, geometry) %>%
      rename(income2020 = hh75inc20)
  } else if (desired_set == "> 75k") {
    data_with_income <- formatted_data %>%
      select(county, hhm7520, geometry) %>%
      rename(income2020 = hhm7520)
  }
  data_shape_obj <- data_with_income %>%
    st_sf()
  return(data_shape_obj)
}

get_income_polygon <- function(map_data, labels, qal) {
  addPolygons(map = map_data, stroke = TRUE, opacity = 1,fillOpacity = 1, smoothFactor = 0.5,
              color="black",
              fillColor = ~qal(income2020),
              weight = 1, 
              label = labels,
              highlight = highlightOptions(
                weight = 3,
                stroke = 4,
                bringToFront = TRUE
              ),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
    addLegend(values=~income2020, pal=qal, title = "Income Percentage", opacity = 1)
}

#endregion


display_text_income <- paste("When looking at properties, taking into account the neighboring homes and what the general income of the area is",
                             " something that could affect your property value. Property value is what someone is willing to pay for a",
                             " property and the amount the seller is willing to accept. If you're looking for a place to live with a higher property",
                             " value, you may be more interested in the counties that have a higher percentage of people making more. This may also be", 
                             " useful if you're looking for areas that make around the same as yourself.", sep = "")

display_text_prop_sales <- paste("One thing to look for when buying a home is the neighborhood comps, or comparable properties in a", 
                                 " specific area that you're looking to buy or sell. One of the best indicators of your homes value is the sale prices of similar",
                                 " homes in your neighborhood. Knowing the average sales price of the county may give you an idea of the general comps and give a",
                                 " better sense of security or a better idea of where to look when working with a relator.", sep = "")

display_text_event_rate <- paste("The calculated event rates per county can give valuable insights as to what a future homeowner may encounter there.",
                                 " The events could range from something as benign as noisy neighbors to frightening events like burglary. The information given by these",
                                 " calculated event rates help the interested home buyer become aware of any possible risks in the area.", sep = "")


#region SHINY APP HANDLERS
ui <- fluidPage(
  
  includeCSS("styling.css"),
  titlePanel("Real Estate Dashboard"),
  
  # can add custom CSS additions later
  # Each tab panel can represent something that we want to showcase with the data
  mainPanel(
    tabsetPanel(type = "tabs",
                #region INCOME MAP PANEL
                tabPanel("Income Map",
                         withTags({
                           div(id = "side-panel-income", checked = NA,
                               selectInput(
                                 
                                 inputId = "dropDownIncome",
                                 label = "Choose an income bracket to observe",
                                 choices = income_levels,
                                 selected = income_levels[1]
                               ),
                               sidebarPanel(
                                 display_text_income
                               )
                           )
                           
                         }),
                         leafletOutput("income_map")
                ),
                #endregion
                
                #region AVG PROPERTY SALES MAP PANEL
                tabPanel("Average property sales price",
                         withTags({
                           div(id = "side-panel-property", checked = NA,
                               sliderInput("range", "Range of sales prices",
                                           min = min_house_sold_price, max = max_house_sold_price,
                                           value = c(min_house_sold_price, max_house_sold_price),
                                           ticks = FALSE
                               ),
                               sidebarPanel(
                                 display_text_prop_sales
                               ))
                         }),
                         leafletOutput("avg_prop_save_price_map")
                ),
                #endregion
                
                #region EVENT RATES PER COUNTY
                tabPanel("Event Rate per County",
                         withTags({
                           div(id = "event-rate-per-county", checked = NA,
                               sidebarPanel(
                                 display_text_event_rate
                               )
                           )
                         }), 
                         leafletOutput("event_rate_map")
                )
                #endregion
    )
  )
)
# Note there's a possible async issue with input.. it doesnt error things out
# might be fixed with req within the observe? not entirely sure
server <- function(input, output, session) {
  
  data_avg_property_sales <- getAvgPropertySaleDatasetFormatted(
    formattedCountyHouseAvg,
    c(min_house_sold_price, max_house_sold_price)
  )
  
  data_income <- getIncomeDatasetFormatted(income_levels[1])
  
  # unlike the others, event data is declared at the top because it does not change due to input
  
  #region INCOME MAP DRAWING
  observe({
    new_income_bracket <- input$dropDownIncome
    data_income <- getIncomeDatasetFormatted(new_income_bracket)
    
    labels_income <- sprintf(
      "<strong>%s</strong><br/>%s%%",
      data_income$county, 
      round(data_income$income2020, 2)
    ) %>% lapply(htmltools::HTML)
    
    # this might not work
    qpal_income <- colorBin("Blues", data_income$income2020, bins=length(income_levels))
    
    output$income_map <- renderLeaflet({
      leaflet(data_income) %>%
        get_income_polygon(labels_income, qpal_income)
    })
    # update the map upon edit
    leafletProxy("income_map", data = data_income) %>%
      clearShapes() %>%
      clearControls() %>%
      get_income_polygon(labels_income, qpal_income)
  })
  #endregion
  
  #region AVERAGE PROPERTY SALES PRICE MAP DRAWING
  observe({ 
    new_bounds_avg_property_sales <- input$range
    
    data_avg_property_sales <- getAvgPropertySaleDatasetFormatted(
      formattedCountyHouseAvg,
      new_bounds_avg_property_sales
    )
    
    labels_avg_property_sale <- sprintf(
      "<strong>%s</strong><br/>$%s",
      data_avg_property_sales$County,
      ceiling(data_avg_property_sales$AVG_SALES)
    ) %>% lapply(htmltools::HTML)
    
    qpal_avg_property_sale <- colorBin("Reds", data_avg_property_sales$AVG_SALES, bins=5)
    
    # If our min is greater than out max, block any updates
    min <- new_bounds_avg_property_sales[1]
    max <- new_bounds_avg_property_sales[2]
    
    if (min < max) {
      
      output$avg_prop_save_price_map <- renderLeaflet({
        leaflet(data_avg_property_sales) %>%
          get_avg_prop_sale_polygon(labels_avg_property_sale, qpal_avg_property_sale)
      })
      
      leafletProxy("avg_prop_save_price_map", data = data_avg_property_sales) %>%
        clearShapes() %>%
        clearControls() %>%
        get_avg_prop_sale_polygon(labels_avg_property_sale, qpal_avg_property_sale)
    }
  })
  #endregion
  
  #region EVENT RATE MAP DRAWING
  observe({
    
    labels_event <- paste(
      sprintf(
        "<strong>%s total event count: %s</strong><br>",
        county_data_st$County,
        county_data_st$event_count
      ),
      "<strong>top 5 events for this county</strong><br>",
      format_tool_tip_event(county_data_st$Event1, county_data_st$Amount1),
      format_tool_tip_event(county_data_st$Event2, county_data_st$Amount2),
      format_tool_tip_event(county_data_st$Event3, county_data_st$Amount3),
      format_tool_tip_event(county_data_st$Event4, county_data_st$Amount4),
      format_tool_tip_event(county_data_st$Event5, county_data_st$Amount5),
      sep = ""
    ) %>% lapply(htmltools::HTML)
    
    qpal_event <- colorBin("Greens", county_data_st$event_count, bins=7)
    
    output$event_rate_map <- renderLeaflet({
      leaflet(county_data_st) %>%
        get_event_rate_polygon(labels_event, qpal_event)
    })

        leafletProxy("event_rate_map", data = county_data_st) %>%
            clearShapes() %>%
            clearControls() %>%
            get_event_rate_polygon(labels_event, qpal_event)
    })
}
#endregion

# Run the app ----0
shinyApp(ui = ui, server = server)
