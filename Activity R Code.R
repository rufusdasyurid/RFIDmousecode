library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)

library(chron)

data<-read.csv('HollyHill_trapdata_clean.csv', header = T)
summary(data)

loggerdata<-read.csv('HollyHill2019_loggerdata_clean.csv', header = T)
summary(loggerdata)
loggerdata$date <- as.POSIXct(loggerdata$date,format="%Y-%m-%d")
loggerdata$datetime <- strptime(loggerdata$datetime, format = "%Y-%m-%d %H:%M:%S")

colnames(loggerdata)[4] ="Date"

sunrise_sunset <- read.csv('SunriseSunset.csv', header = T)
sunrise_sunset$Date <- as.POSIXct(sunrise_sunset$Date,format="%m/%d/%Y")
sunrise_sunset$SunRise <- strptime(sunrise_sunset$SunRise..Date.Time., format = "%d/%m/%Y %H:%M:%S")
sunrise_sunset$SunSet <- strptime(sunrise_sunset$SunSet..Date.Time., format ="%d/%m/%Y %H:%M:%S")
colnames(sunrise_sunset)[3] ="SunRise"
colnames(sunrise_sunset)[6] ="SunSet"

cleaned_wytham<-loggerdata
### add new column to define, day or light hits ####
head(loggerdata)
i = 92

cleaned_wytham$light <- NA
for (i in 1: nrow(cleaned_wytham)){
  capturetime <- cleaned_wytham$datetime[i]
  capturedate <- cleaned_wytham$Date[i]
  sunset <- sunrise_sunset$sunSet[sunrise_sunset$Date == capturedate]
  sunrise <- sunrise_sunset$sunRise[sunrise_sunset$Date == capturedate]
  if (capturetime > sunrise & capturetime < sunset) { cleaned_wytham$light[i] = "1"}
  else{ cleaned_wytham$light[i] = "0"}
  print(i)
}
