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
library(leaflet)
library(rgdal)
library(stringr)
library(sf)
library(spData)
library(sp)
library(maps)
library(maptools)
library(plotly)
library(usmap)

big_data <- fread("Data/restaurantDataVisProjData.csv")

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
latlong2county <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per county
    counties <- map("county", fill = TRUE, col = "transparent", plot = FALSE)
    IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
    counties_sp <- map2SpatialPolygons(counties,
        IDs = IDs,
        proj4string = CRS("+proj=longlat +datum=WGS84")
    )

    # Convert pointsDF to a SpatialPoints object
    pointsSP <- SpatialPoints(pointsDF,
        proj4string = CRS("+proj=longlat +datum=WGS84")
    )

    # Use 'over' to get _indices_ of the Polygons object containing each point
    indices <- over(pointsSP, counties_sp)

    # Return the county names of the Polygons object containing each point
    countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
    countyNames[indices]
}

# gather entries of [state,county], store them as a list then use purrr to split the list and assign
# the respective values to big_data
lat_lon_to_county <- (latlong2county(data.frame(x = big_data$longitude, y = big_data$latitude)))
big_data$business_state <- purrr::map(strsplit(lat_lon_to_county$state_county, ","), ~ .x[1]) %>% unlist()
big_data$business_county <- purrr::map(strsplit(lat_lon_to_county$state_county, ","), ~ .x[2]) %>% unlist()


# group the data for numerical values
big_data_grouped <- big_data %>%
    group_by(business_state, business_county) %>%
    count() %>%
    filter(!is.na(business_state))

# convert a state,county to a FIPS
# if the conversion errors (invalid county), then return NA
# else if the county maps to more than one FIPS, return the first
convert_to_fips <- function(state, county) {
    conversion <- try(fips(state = first(state), county = county))
    if (class(conversion) == "try-error") {
        return(NA)
    }
    if (length(conversion == 2)) {
        return(conversion[1])
    }
    print(conversion)
    return(conversion)
}

big_data_grouped <- big_data_grouped %>% 
  group_by(business_state) %>% 
  mutate(new = convert_to_fips(business_state, business_county))


url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df = read.csv("https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv", header = T, colClasses = c("fips"="character"))
df <- read.csv(url2, colClasses=c(fips="character"))
fig <- plot_ly() 
fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=counties,
    locations=big_data_grouped$new,
    z=big_data_grouped$n,
    colorscale="Viridis",
    zmin=0,
    zmax=12,
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom =2,
      center=list(lon= -95.71, lat=37.09))
  )
fig
