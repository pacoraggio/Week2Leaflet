# mainScript
rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
# loading tided raw data

load('rawdata.RData')

df.capitals <- read.csv("./Data/state.csv")
df.code <- read.csv("./Data/countrycode.csv")
df.capitals$STATE <- df.code$Country.Code

# for each state compute
# - mean of fatalities during the YEARS
# - median
# - sd
# - max and year where max has been registred

data <- df.rawdata %>%
    group_by(STATE, YEAR) %>%
    summarise(sfatalities.py = sum(FATALITIES))

data <- data %>%
    group_by(STATE) %>%
    summarise(sum.fatalities = sum(sfatalities.py),
              mean.fatalities = mean(sfatalities.py),
              median.fatalities = median(sfatalities.py),
              sd.fatalities = sd(sfatalities.py),
              max.fatalities = max(sfatalities.py),
              year.max = paste(YEAR[which(sfatalities.py == max(sfatalities.py))], collapse = ", "))


## Test with AK

# df.Ak = df.rawdata[df.rawdata$STATE == "AK",]
# AK.year <- df.AK %>%
#     group_by(YEAR) %>%
#     summarise(s = sum(FATALITIES))
# 
# plot(AK.year$YEAR, AK.year$s)
# AK.year$YEAR[which(AK.year$s == max(AK.year$s))]

df.plotdata <- merge(df.capitals, data, by = "STATE")
head(df.plotdata)

fatalitiesIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Injury_icon_2.svg/1024px-Injury_icon_2.svg.png",
    iconWidth = 11*215/230, iconHeight = 11,
    iconAnchorX = 11*215/230/2, iconAnchorY = 6
)


labels <- sprintf(
    "<strong>%s</strong><br/> sum: %g<br/> mean: %g <br/> median: %g, <br/> max: %g in: %s",
    df.plotdata$State,
    df.plotdata$sum.fatalities,
    df.plotdata$mean.fatalities, 
    df.plotdata$median.fatalities,
    df.plotdata$max.fatalities,
    df.plotdata$year.max
) %>% lapply(htmltools::HTML)


median(df.plotdata$mean.fatalities)

icon.fa <- makeAwesomeIcon(icon = 'flag', 
                           markerColor = ifelse(df.plotdata$mean.fatalities < 4.6, 'green','red'), 
                           library='fa',
                           iconColor = 'white')

# icon.pop <- awesomeIcons(icon = icon.fa,
#                          markerColor = ifelse(df.plotdata$mean.fatalities < 4.6,
#                                               'green','red'),
#                          library = 'fa',
#                          iconColor = 'black')

icon.pop <- awesomeIcons(icon = icon.fa,
                         library = 'fa',
                         iconColor = 'black')

# head(df.plotdata)
# df.plotdata %>%
#     leaflet() %>%
#     addTiles() %>%
#     addCircles(weight = 1.7, 
#                radius = df.plotdata$sum.fatalities *200,
#                fillColor = "red",
#                color = "black",
#                fillOpacity = df.plotdata$mean.fatalities*0.04) %>%
#     # addMarkers(icon = fatalitiesIcon, 
#     #            label = paste("mean fatalities: ", 
#     #                          round(df.plotdata$mean.fatalities, 2)))
#     addAwesomeMarkers(icon = icon.fa, 
#                label = labels) %>%
#     addLegend(labels = c("mean fatalities < 4.6 per year",
#                          "mean fatalities > 4.6 per year"),
#               colors = c("green", "red"))
# 

lat1 <- min(df.capitals$Latitude) 
lat2 <- max(df.capitals$Latitude)
lon1 <- min(df.capitals$Longitude)
lon2 <- max(df.capitals$Longitude)


library(maps)
mapStates = map("state", fill = TRUE,plot = FALSE)

df.plotdata %>%
    leaflet(options = leafletOptions(minZoom = 2.8)) %>%
    addTiles() %>%
    addCircles(weight = 1.7, 
               radius = df.plotdata$sum.fatalities *200,
               fillColor = "red",
               color = "black",
               fillOpacity = df.plotdata$mean.fatalities*0.04) %>%
    addAwesomeMarkers(icon = icon.fa, 
                      label = labels) %>%
    addLegend(labels = c("mean fatalities < 4.6 per year",
                         "mean fatalities > 4.6 per year"),
              colors = c("green", "red")) %>%
    setView(lng = lon1 -20,
            lat = lat1 -10,
            zoom = 2.8) %>%
    setMaxBounds( lng1 = lon1 - 10
                  , lat1 = lat1 - 5
                  , lng2 = lon2 + 10
                  , lat2 = lat2 + 10)

?setMaxBounds
library(maps)
mapStates = map("state", plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
    addPolygons(stroke = FALSE)
