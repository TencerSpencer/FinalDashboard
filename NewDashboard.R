library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(dplyr)
library(usmap)
library(tidyverse)
library(treemap)
library(sunburstR)
library(leaflet)
library(rgdal)
library(stringr)
library(sf)
library(spData)
library(sp)
library(maps)
library(maptools)
# Circle packing specific libraries
library(data.tree)
library(hrbrthemes)
devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)   

# data loading
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

finalData <- read.csv("Data/restaurantsWithRegions.csv")

# region MAIN MAP DATA FORMATTING
## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC.
##
## name_col: Name of a column in `states` that supplies the states'
##           names.
lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
    ## Convert points data.frame to an sf POINTS object
    pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)

    ## Transform spatial data to some planar coordinate system
    ## (e.g. Web Mercator) as required for geometric operations
    states <- st_transform(states, crs = 3857)
    pts <- st_transform(pts, crs = 3857)

    ## Find names of state (if any) intersected by each point
    state_names <- states[[name_col]]
    ii <- as.integer(st_intersects(pts, states))
    state_names[ii]
}

# front load expensive operations
states <- readOGR("Data/cb_2019_us_state_5m.shp")
finalData$business_state <- lonlat_to_state(data.frame(x = finalData$longitude, y = finalData$latitude))
unique_business_states <- finalData %>%
    distinct(business_state) %>%
    filter(!is.na(business_state))
# endregion

# region CIRCLE PACKING DATA FORMATTING
unique_attributes <- colnames(finalData)[15:26]
unique_styles <- finalData %>%
    select(Style) %>%
    filter(Style != 'Undefined') %>%
    filter(!is.na(Style)) %>%
    as.list()
unique_styles <- c("None", unlist(unique_styles$Style))

yelpCircle <- yelpData <- finalData %>%
  filter(Region != 'Bar' 
         & Region != 'Coffee/Juice/Tea') %>%
  dplyr::select(stars, Region, Style, review_count) %>%
  droplevels() 

# Change the format. This use the data.tree library. This library needs a column that looks like root/group/subgroup/
yelpCircle$pathString <- paste("Resturaunt Ratings", yelpCircle$stars, yelpCircle$Region, yelpCircle$Style, sep = "/")
restaurant <- as.Node(yelpCircle)
# endregion

ui <- dashboardPage(
    dashboardHeader(title = "Help find your next favorite resturant"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("My control Center",
                tabName = "control",
                icon = icon("dashboard")
            ),
            menuItem("Food Origin",
                tabName = "food_origin",
                icon = icon("th")
            )
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "control",
                h2("Which city are you most likely to pick as your go to?"),
                fluidRow(
                    box(
                        column(
                            width = 4,
                            wellPanel(
                                selectInput(
                                    inputId = "stateSelectInput",
                                    label = "state to display",
                                    choices = unique_business_states
                                ),
                                sliderInput(
                                    inputId = "averageStarsSlider",
                                    label = "Average star count",
                                    min = 0,
                                    max = 5,
                                    value = c(0, 5),
                                    step = 0.1
                                ),
                            )
                        ),
                        column(
                            width = 5,
                            selectInput(
                                inputId = "filterByAttribute",
                                label = "filter by attribute",
                                choices = c("false", "true")
                            ),
                            selectInput(
                                inputId = "filterByChainOrNot",
                                label = "choose to search for a chain or not, or both",
                                choices = c("both", "yes", "no")
                            ),
                            selectInput(
                                input = "filterByStyle",
                                label = "choose to search by style",
                                choices = unique_styles

                            )
                        ),
                        column(
                            width = 5,
                            conditionalPanel(
                                condition = "input.filterByAttribute == 'true'",
                                wellPanel (
                                    checkboxGroupInput(
                                        inputId = "attributesCheckboxes",
                                        label = "customize desired attributes",
                                        choices = unique_attributes,
                                        selected = c()
                                    )
                                )
                            )
                        )
                    ),
                    leafletOutput("resturant", height = 1200)
                )
            ),
            # Second tab content
            tabItem(
                tabName = "food_origin",
                h2("What is a star rating's most popular food origin"),
                fluidRow(
                  box(circlepackeROutput("packedCircle", width = "100%", height = "1000px"),
                      height = 1000,
                      width = 600,
                      maximizable = T,
                      align = "center")
                )
            )
        )
    )
)

server <- function(input, output) {
    output$packedCircle <- renderSunburst({
      circlepackeR(restaurant, size = "review_count", color_min = "hsl(56,80%,80%)", color_max = "hsl(341,30%,40%)")
    })

    observe({
        min_stars <- input$averageStarsSlider[1]
        max_stars <- input$averageStarsSlider[2]

        finalData$business_state <- lonlat_to_state(data.frame(x = finalData$longitude, y = finalData$latitude))
        finalData <- finalData %>%
            filter(business_state == input$stateSelectInput) %>%
            filter(average_stars >= min_stars & average_stars <= max_stars)

        if (input$filterByAttribute == "true") {
            finalData <- finalData %>%
                filter(RestaurantsReservations == ("RestaurantsReservations" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(RestaurantsDelivery == ("RestaurantsDelivery" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(HappyHour == ("HappyHour" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(RestaurantsTakeOut == ("RestaurantsTakeOut" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(OutdoorSeating == ("OutdoorSeating" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(BusinessAcceptsCreditCards == ("BusinessAcceptsCreditCards" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(Open24Hours == ("Open24Hours" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(Alcohol == ("Alcohol" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(DriveThru == ("DriveThru" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(DogsAllowed == ("DogsAllowed" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(RestaurantsTableService == ("RestaurantsTableService" %in% input$attributesCheckboxes == TRUE)) %>%
                filter(RestaurantsGoodForGroups == ("RestaurantsGoodForGroups" %in% input$attributesCheckboxes == TRUE))
        }
        if (input$filterByChainOrNot == "yes" | input$filterByChainOrNot == "no") {
            finalData <- finalData %>%
                filter(is_chain == input$filterByChainOrNot)
        } 
        if (input$filterByStyle != "None") {
            finalData <- finalData %>%
                filter(Style == input$filterByStyle)
        }

        state_geojson <- subset(states, NAME == input$stateSelectInput)

        if (length(row.names(finalData)) != 0) {
            output$resturant <- renderLeaflet({
                leaflet(data = finalData, options = leafletOptions(preferCanvas = TRUE)) %>%
                    addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(
                        updateWhenZooming = FALSE,
                        updateWhenIdle = FALSE,
                    )) %>%
                    setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
                    addPolygons(
                        data = state_geojson,
                        weight = 1
                    ) %>%
                    addMarkers(~longitude, ~latitude, clusterOptions = markerClusterOptions(maxClusterRadius = 20), label = finalData$name)
            })
        }
    })
}

shinyApp(ui, server)
