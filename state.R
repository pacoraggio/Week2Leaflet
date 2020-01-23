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


df.capitals <- read.csv("./Data/state.csv")
df.code <- read.csv("./Data/countrycode.csv")
df.capitals$StateCode <- df.code$Country.Code

# t1 <- t1[t1$STATE %in% df.capitals$StateCode,]
df.rawdata <- df.rawdata[df.rawdata$STATE %in% df.capitals$StateCode,]

df.rawdata$YEAR <- year(as.Date(sub(" .*", 
                            "", 
                            df.rawdata$BGN_DATE), 
                        format("%m/%d/%Y")))

head(df.rawdata)

save(df.rawdata, file = "rawdata.RData")

data <- df.rawdata %>%
    group_by(STATE, YEAR) %>%
    summarise(sum_fatalities = sum(FATALITIES),
              sum_injuries = sum(INJURIES),
              total_casualties = sum_fatalities + sum_injuries)

head(data)
data %>% group_by(Group) %>%
    summarize(minAge = min(Age), minAgeName = Name[which.min(Age == min(Age))], 
              maxAge = max(Age), maxAgeName = Name[which.max(Age == max(Age))])

data %>% group_by(STATE) %>%
    summarise(max_c = max(sum_fatalities),
              maxYear = YEAR[which.max(sum_fatalities == max(sum_fatalities))])
    
datamaxfat <- data %>% group_by(STATE) %>%
    summarize(max_c = max(sum_fatalities), 
              maxYear = paste(YEAR[which(sum_fatalities == max(sum_fatalities))], collapse = ", "))


################ 
data %>%
    group_by(STATE) %>%
    summarise(max_fatalities = max(sum_fatalities), 
              maxyear = data[which.max(data$sum_fatalities == max(data$sum_fatalities)),]$YEAR)




data[which.max(data$sum_fatalities == max(data$sum_fatalities)),]$YEAR
head(data)
m_fatalities <- data %>%
    group_by(STATE) %>%             
    summarise(max_fatalities = max(sum_fatalities))


data[data$STATE == "AK" & data$sum_fatalities == 18,]$YEAR

max_fatalities

leaflet()

df.capitals %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers()

t1$YEAR <- year(as.Date(sub(" .*", 
                                  "", 
                                  t1$BGN_DATE), 
                              format("%m/%d/%Y")))

t1 <- t1[!(t1$FATALITIES ==0 | t1$)]
