# Load the packages
library(leaflet)
library(htmlwidgets)
library(htmltools)

# Read the data
eu_df <- read.csv("eu_states.csv")

# Format the Popup
eu_popup <- paste0("<a href='", eu_df$Links, "'>",
                   "State: ", eu_df$State,
                   "; Capital: ", eu_df$Capital,
                   "; Population Density:", eu_df$Population_Density,
                   "(per km^2)</a>") 

# Add the Icon
eu_icon <- makeIcon(iconUrl = eu_df$Flag,
                    iconWidth = 50,
                    iconAnchorX = 25,
                    iconAnchorY = 15)

# Format Title
tag.map.title <- tags$style(HTML(".leaflet-control.map-title
                                    {transform: translate(-50%,20%);
                                    position: fixed !important;
                                    left: 50%;
                                    text-align: center;
                                    padding-left: 10px;
                                    padding-right: 10px;
                                    background: rgba(255,255,255,0.75);
                                    font-weight: bold;
                                    font-size: 28px;}"))
# Add the Title
title <- tags$div(tag.map.title,
                  HTML("Europen Union States with respect to Population Density"))

# Construct the map
map_leaflet <- eu_df %>%
                leaflet() %>%
                addTiles() %>%
                addMarkers(lat = eu_df$Latitude,
                           lng = eu_df$Longitude,
                           icon = eu_icon,
                           popup = eu_popup,
                           clusterOptions = markerClusterOptions()) %>%
                addCircleMarkers(popup = eu_popup,
                                 weight = 2, 
                                 radius = 10 * log(as.numeric(as.character(eu_df$Population_Density))),
                                 clusterOptions = markerClusterOptions()) %>%
                addControl(title,
                           position = "topleft", className="map-title")

# Export the map to a html document
saveWidget(map_leaflet, file="eu_states.html")