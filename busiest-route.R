# Mapping world's busiest international airline route
# Data gathered from https://en.wikipedia.org/wiki/List_of_busiest_passenger_air_routes
# Tombayu Amadeo Hidayat
# 6 October 2018

rm(list = ls())
setwd("C:/Users/Tombayu Hidayat/Documents/Coding/Spatially/Shipping Map/busiest-route")

library(tidyverse)
library(rvest)
library(leaflet)
library(sp)
library(raster)
library(geosphere)

url <- "https://en.wikipedia.org/wiki/List_of_busiest_passenger_air_routes"

# Use # in front of 'id' element
# Use . in front of 'class' element
# Use a, div, h1, h2, h3, or body to get into that chunk

### WEBSCRAPING ###

routeData <- url %>%
  read_html() %>%
  html_nodes("body #content #bodyContent #mw-content-text .mw-parser-output table") %>% # Obtained from css selector tool
  html_table(fill = T)

# All of the tables available in the webpage has been downloaded! But we are more interested in this table
# https://en.wikipedia.org/wiki/List_of_busiest_passenger_air_routes#International
international <- routeData[[19]]

# Now some data cleaning..
# We don't need the rank, so we delete them
international <- international[2:length(international)]
# Rename the columns
names(international) <- c("City 1", "City 2", "Flights 2018", "Flights 2017", "Operators")
# Notice the errors in thousands division in flight numbers? Some with (,), but there is also one with (.)
# Let's remove them and convert into numeric type
international$`Flights 2018` <- as.numeric(gsub(",|\\.", "", international$`Flights 2018`))
international$`Flights 2017` <- as.numeric(gsub(",|\\.", "", international$`Flights 2017`))
# City names have to be as general as possible: constraints of geocoding using dsk
international$`City 1` <- gsub("-.*|\u2013.*", "", international$`City 1`) # Em dash (-) has the code \u2013
international$`City 2` <- gsub("-.*|\u2013.*", "", international$`City 2`)

# Split the data..
flights2017 <- international[,c(1:2, 4:5)]
flights2017 <- flights2017[complete.cases(flights2017),]
flights2018 <- international[,c(1:2, 3, 5)]
flights2018 <- flights2018[complete.cases(flights2018),]

### GEOCODING ###

# Geocoding the data
cities <- as.character(unique(c(international$`City 1`, international$`City 2`)))
geocodes <- ggmap::geocode(cities, output = "latlon", source = "dsk")
geocodedCity <- data.frame("city" = cities, "lon" = geocodes$lon, "lat" = geocodes$lat)

# Join the coordinates
flights2017 <- left_join(flights2017, geocodedCity, by = c("City 1" = "city"))
flights2017 <- left_join(flights2017, geocodedCity, by = c("City 2" = "city"))
names(flights2017) <- c("City 1", "City 2", "Flights 2017", "Operators", "Lon1", "Lat1", "Lon2", "Lat2")
flights2018 <- left_join(flights2018, geocodedCity, by = c("City 1" = "city"))
flights2018 <- left_join(flights2018, geocodedCity, by = c("City 2" = "city"))
names(flights2017) <- c("City 1", "City 2", "Flights 2018", "Operators", "Lon1", "Lat1", "Lon2", "Lat2")

write.csv(flights2017, "route2017.csv")
write.csv(flights2018, "route2018.csv")

### MAPPING ###

# Making city points
# 2018 only
top2018 <- data.frame("city" = unique(c(flights2018$`City 1`, flights2018$`City 2`)))
top2018 <- left_join(top2018, geocodedCity)

# Making lines connecting the routes
# Making points constructing the line using gcIntermediate
# And then convert them into SpatialLine object
lineList <- list()
for (i in 1:length(flights2018[,1])) {
  city1 <- c(flights2018$lon.x[i], flights2018$lat.x[i])
  city2 <- c(flights2018$lon.y[i], flights2018$lat.y[i])
  lineList[[i]] <- spLines(gcIntermediate(city1, city2, n = 100, addStartEnd = T))
}

routes <- do.call("rbind", lineList)
routes <- SpatialLinesDataFrame(sl = routes, data = flights2018, match.ID = F)

map <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = top2018, ~lon, ~lat, label = ~city) %>%
  addPolylines(data = routes,
               weight = ~(20*(`Flights 2018`/max(flights2018$`Flights 2018`))),
               opacity = 0.3,
               highlight = highlightOptions(
                 color = "#666",
                 opacity = 1
               ),
               label = ~paste0(`City 1`, " to ", `City 2`))

map
