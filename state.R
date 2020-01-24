rm(list = ls())
library(leaflet)
library(dplyr)
library(ggplot2)
library(lubridate)

destfile <- "./Data/repdata_data_StormData.csv.bz2"
if(!file.exists(destfile))
{
    dir.create("./Data")
    destfile <- "./Data/repdata_data_StormData.csv.bz2"
    urlziplocation <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(urlziplocation, destfile)
    names <- read.csv(destfile, nrows = 5, header = TRUE)
}else{
    names <- read.csv(destfile, nrows = 5, header = TRUE)
}

names(names)

df.rawdata <- read.csv("./Data/repdata_data_StormData.csv.bz2",
                       colClasses = c("NULL", 
                                      "character", 
                                      rep("NULL",4), 
                                      rep("character",2),
                                      rep("NULL",14), 
                                      rep("numeric",2), 
                                      rep("NULL",7),
                                      rep("numeric",2),
                                      rep("NULL", 4)))


names(df.rawdata)

df.rawdata <- df.rawdata[!(df.rawdata$FATALITIES == 0 &
                               df.rawdata$INJURIES == 0),]


# t1 <- t1[t1$STATE %in% df.capitals$StateCode,]
df.rawdata <- df.rawdata[df.rawdata$STATE %in% df.capitals$StateCode,]

df.rawdata$YEAR <- year(as.Date(sub(" .*", 
                            "", 
                            df.rawdata$BGN_DATE), 
                        format("%m/%d/%Y")))

head(df.rawdata)

save(df.rawdata, file = "rawdata.RData")
load("rawdata.RData")

df.capitals <- read.csv("./Data/state.csv")
df.code <- read.csv("./Data/countrycode.csv")
df.capitals$STATE <- df.code$Country.Code

data <- df.rawdata %>%
    group_by(STATE) %>%
    summarise(mean_fatalities = mean(FATALITIES),
              mean_injuries = mean(INJURIES),
              total_casualties = mean_fatalities + mean_fatalities)

head(data)
# data %>% group_by(Group) %>%
#     summarize(minAge = min(Age), minAgeName = Name[which.min(Age == min(Age))], 
#               maxAge = max(Age), maxAgeName = Name[which.max(Age == max(Age))])

data %>% group_by(STATE) %>%
    summarise(max_c = max(mean_fatalities),
              maxYear = YEAR[which.max(mean_fatalities == max(sum_fatalities))])
    
datamaxfat <- data %>% group_by(STATE) %>%
    summarize(max_c = max(sum_fatalities), 
              maxYear = paste(YEAR[which(sum_fatalities == max(sum_fatalities))], collapse = ", "))

leaflet()
df.plotdata <- merge(df.capitals, data, by = "STATE")


df.capitals %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers()


# https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Injury_icon_2.svg/1024px-Injury_icon_2.svg.png
# https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines

fatalitiesIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Injury_icon_2.svg/1024px-Injury_icon_2.svg.png",
    iconWidth = 11*215/230, iconHeight = 11,
    iconAnchorX = 11*215/230/2, iconAnchorY = 6
)


df.plotdata %>%
    leaflet() %>%
    addTiles() %>%
    addCircles(weight = 1.7, 
               radius = df.plotdata$mean_fatalities *100000,
               fillColor = "red",
               color = "black",
               fillOpacity = df.plotdata$mean_fatalities*0.75) %>%
    addMarkers(icon = fatalitiesIcon, 
               label = paste("mean fatalities: ", 
                             round(df.plotdata$mean_fatalities, 2)))

    