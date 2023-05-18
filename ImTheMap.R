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

## Test the function with points in Wisconsin, Oregon, and France
testPoints <- data.frame(x = c(-90, -120, 0), y = c(44, 44, 44))
lonlat_to_state(testPoints)


big_data <- fread("Data/restaurantDataVisProjData.csv")

# THIS WILL PULL EACH STATE'S NAME
# https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
big_data$business_state <- lonlat_to_state(data.frame(x = big_data$longitude, y = big_data$latitude))
big_data_california <- big_data %>%
    filter(business_state == "California")


states <- readOGR('Data/cb_2019_us_state_5m.shp')

california <- subset(states, NAME == "California")

m <- leaflet(data = big_data_california, options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions( 
    updateWhenZooming = FALSE,
    updateWhenIdle = FALSE,
  ))  %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = california,
              weight = 1) %>%
  addMarkers(~longitude, ~latitude, clusterOptions = markerClusterOptions(maxClusterRadius = 20))

m


###########################################################################
# CHLOROPATH COUNTY MAP WORK



# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
latlong2county <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per county
    counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
    counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                     proj4string=CRS("+proj=longlat +datum=WGS84"))

    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                    proj4string=CRS("+proj=longlat +datum=WGS84"))

    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, counties_sp)

    # Return the county names of the Polygons object containing each point
    countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
    countyNames[indices]
}

# gather entries of [state,county], split them inside their own DF then assign their respective fields
# to the big_data DF
lat_lon_to_county <- as.data.frame(latlong2county(data.frame(x = big_data$longitude, y = big_data$latitude)))
colnames(lat_lon_to_county) <- c('state_county')
lat_lon_to_county <- lat_lon_to_county %>%
    mutate(business_state = str_split(state_county, ',')[[1]][1]) %>%
    mutate(business_county = str_split(state_county, ',')[[1]][2])

big_data$business_state <- lat_lon_to_county$business_state
big_data$business_county <- lat_lon_to_county$business_county
