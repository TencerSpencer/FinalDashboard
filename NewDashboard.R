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

# data loading
finalData <- read.csv("Data/restaurantDataVisProjData.csv")
stylesData <- read.csv("Data/Food_Origins.csv")

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

# region SUNBURST DATA FORMATTING
stylesData <- stylesData %>%
    rename_with(.cols = 1, ~"business_id")

test <- finalData %>%
    left_join(stylesData, join_by("business_id"))

unique_attributes <- colnames(finalData)[15:26]

finalData <- test %>%
    select(
        business_id, name, address, latitude, longitude, states, stars, average_stars, review_count,
        total_review_count, count, is_chain, Style, Region, RestaurantsReservations, RestaurantsDelivery,
        HappyHour, RestaurantsTakeOut, OutdoorSeating, BusinessAcceptsCreditCards, DriveThru, Open24Hours,
        DogsAllowed, Alcohol, RestaurantsTableService, RestaurantsGoodForGroups, Monday, Tuesday, Wednesday,
        Thursday, Friday, Saturday, Sunday
    )

yelpData <- finalData %>%
    filter(Style != "Undefined" &
        Region != "Bar" &
        Region != "Coffee/Juice/Tea" &
        Style != "Alcohol" &
        Style != "NonAlcoholicDrinks") %>%
    mutate(stars = floor(stars)) %>%
    mutate(path = paste(stars, Region, Style, Style, sep = "-")) %>%
    dplyr::select(path, stars)
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
                            width = 5,
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
                                checkboxGroupInput(
                                    inputId = "attributesCheckboxes",
                                    label = "customize desired attributes",
                                    choices = unique_attributes,
                                    selected = unique_attributes
                                )
                            )
                        )
                    ),
                    leafletOutput("resturant", height = 600)
                ),
            ),
            # Second tab content
            tabItem(
                tabName = "food_origin",
                h2("Most middle rated resturants are of North American Type"),
                fluidRow(
                    box(sunburstOutput("sunburst"), height = 800, width = 600)
                )
            )
        )
    )
)

server <- function(input, output) {
    output$sunburst <- renderSunburst({
        sunburst(yelpData, legend = TRUE)
    })

    observe({

        print(input$attributesCheckboxes)

        min_stars <- input$averageStarsSlider[1]
        max_stars <- input$averageStarsSlider[2]

        finalData$business_state <- lonlat_to_state(data.frame(x = finalData$longitude, y = finalData$latitude))
        state_data <- finalData %>%
            filter(business_state == input$stateSelectInput) %>%
            filter(average_stars >= min_stars & average_stars <= max_stars) %>%
            filter(RestaurantsReservations == ("RestaurantsReservations" %in% input$attributesCheckboxes)) %>%
            filter(RestaurantsDelivery == ("RestaurantsDelivery" %in% input$attributesCheckboxes)) %>%
            filter(HappyHour == ("HappyHour" %in% input$attributesCheckboxes == TRUE))

            # make attribute mode some sort of toggle or have to manually add them to the list

        state_geojson <- subset(states, NAME == input$stateSelectInput)

        output$resturant <- renderLeaflet({
            leaflet(data = state_data, options = leafletOptions(preferCanvas = TRUE)) %>%
                addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(
                    updateWhenZooming = FALSE,
                    updateWhenIdle = FALSE,
                )) %>%
                setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
                addPolygons(
                    data = state_geojson,
                    weight = 1
                ) %>%
                addMarkers(~longitude, ~latitude, clusterOptions = markerClusterOptions(maxClusterRadius = 20), label = state_data$name)
        })
    })
}

shinyApp(ui, server)