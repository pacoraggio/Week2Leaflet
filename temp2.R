## Choropleths
## https://rstudio.github.io/leaflet/choropleths.html
# From http://leafletjs.com/examples/choropleth/us-states.js

rm(list = ls())

library(geojson)
library(geojsonio)
url <- "http://leafletjs.com/examples/choropleth/us-states.js"

# read as text file
doc <- readLines(url)

# remove the javascript assignment at the front 
doc2 <- gsub("var statesData = ", "", doc)

# write out as a temp file and read
write(doc2, file = "tempgeo.json")
states <- geojson_read("tempgeo.json", what = "sp")

names(states)

m <- leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m %>% addPolygons()

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)

m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE))


labels <- sprintf(
    "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
    states$name, states$density
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
m