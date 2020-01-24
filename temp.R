# https://www.r-bloggers.com/interactive-mapping-with-leaflet-in-r/
    
states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
class(states)

rm(list = ls())

Name <- c("Sam", "Sarah", "Jim", "Fred", "James", "Sally", "Andrew", "John", "Mairin", "Kate", "Sasha", "Ray", "Ed")
Age <- c(22,31,31,35,58,82,17,34,12,24,44,67,43)
Group <- c("A", "B", "B", "B", "B", "C", "C", "D", "D", "D", "D", "D", "D") 
data <- data.frame(Name, Age, Group)

data %>% group_by(Group) %>%
    summarize(minAge = min(Age), minAgeName = Name[which.min(Age == min(Age))], 
              maxAge = max(Age), maxAgeName = Name[which.max(Age == max(Age))])

data %>% group_by(Group) %>%
    summarize(minAge = min(Age), minAgeName = paste(Name[which(Age == min(Age))], collapse = ", "), 
              maxAge = max(Age), maxAgeName = paste(Name[which(Age == max(Age))], collapse = ", "))

### 
library(leaflet)

icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue',
                                  iconColor = 'black', library = 'glyphicon',
                                  squareMarker =  TRUE)
icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa',
                           iconColor = 'black')
icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green',
                            library='ion')


# Marker + Label
leaflet() %>% addTiles() %>%
    addAwesomeMarkers(
        lng=-118.456554, lat=34.078039,
        label='This is a label',
        icon = icon.glyphicon)

leaflet() %>% addTiles() %>%
    addAwesomeMarkers(
        lng=-118.456554, lat=34.078039,
        label='This is a label',
        icon = icon.fa)

leaflet() %>% addTiles() %>%
    addAwesomeMarkers(
        lng=-118.456554, lat=34.078039,
        label='This is a label',
        icon = icon.ion)

# Marker + Static Label using custom label options
leaflet() %>% addTiles() %>%
    addAwesomeMarkers(
        lng=-118.456554, lat=34.078039,
        label='This is a static label',
        labelOptions = labelOptions(noHide = T),
        icon = icon.fa)


cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))

library(dplyr)
cities <- cities %>% mutate(PopCat=ifelse(Pop <500000,'blue','red'))


leaflet(cities) %>% addTiles() %>%
    addAwesomeMarkers(lng = ~Long, lat = ~Lat,
                      label = ~City,
                      icon = icon.ion)

icon.pop <- awesomeIcons(icon = 'users',
                         markerColor = ifelse(cities$Pop <500000,'blue','red'),
                         library = 'fa',
                         iconColor = 'black')

leaflet(cities) %>% addTiles() %>%
    addAwesomeMarkers(lng = ~Long, lat = ~Lat,
                      label = ~City,
                      icon = icon.pop)

# Make a list of icons (from two different icon libraries).
# We'll index into it based on name.
popIcons <- awesomeIconList(
    blue = makeAwesomeIcon(icon='user', library='glyphicon', markerColor = 'blue'),
    red = makeAwesomeIcon(icon='users', library='fa', markerColor = 'red'))

leaflet(cities) %>% addTiles() %>%
    addAwesomeMarkers(lng = ~Long, lat = ~Lat,
                      label = ~City,
                      labelOptions = rep(labelOptions(noHide = T),nrow(cities)),
                      icon = ~popIcons[PopCat] )



library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(leaflet)
library(scales)
### Begin data prep
# Grab air/water quality data from the EPA
url = "https://data.cdc.gov/api/views/cjae-szjv/rows.csv?accessType=DOWNLOAD"
dat <- read.csv(url, stringsAsFactors = FALSE)
# Colnames tolower
names(dat) <- tolower(names(dat))
dat$countyname <- tolower(dat$countyname)
# Wide data set, subset only what we need.
county_dat <- subset(dat, measureid == "296", 
                     select = c("reportyear","countyfips","statename", "countyname", "value", "unitname")) %>%
    subset(reportyear==2011, select = c("countyfips", "value"))
# Rename columns to make for a clean df merge later.
colnames(county_dat) <- c("GEOID", "airqlty")
# Have to add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
# I'm cheating by using C code. sprintf will work as well.
county_dat$GEOID <- formatC(county_dat$GEOID, width = 5, format = "d", flag = "0")
### End data prep

# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
us.map <- readOGR(dsn = ".", 
                  layer = "cb_2013_us_county_20m", 
                  stringsAsFactors = FALSE)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]
# Merge spatial df with downloade ddata.
leafmap <- merge(us.map, county_dat, by=c("GEOID"))

# Format popup data for leaflet map.
popup_dat <- paste0("<strong>County: </strong>", 
                    leafmap$NAME, 
                    "<br><strong>Value: </strong>", 
                    leafmap$airqlty)

pal <- colorQuantile("YlOrRd", NULL, n = 20)
# Render final map in leaflet.
leaflet(data = leafmap) %>% addTiles() %>%
    addPolygons(fillColor = ~pal(airqlty), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1,
                popup = popup_dat)