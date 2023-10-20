library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)
library(chron)

##### load data #####
sunrise_sunset <- read.csv('SunriseSunset.csv', header = T)
silwood_actogram <- read.csv('HollyHill2019_Loggerdata_clean.csv', header = T)
colnames(silwood_actogram)[4] ="Date"
colnames(silwood_actogram)[5] ="DateTime"

##### clean silwood ####
silwood_actogram$Date <- as.Date(silwood_actogram$Date, format = "%Y-%m-%d")
cleaned_silwood <- silwood_actogram
cleaned_silwood$DateTime <- as.POSIXct(cleaned_silwood$DateTime,  format = "%Y-%m-%d %H:%M:%S", tz="GMT")
class(cleaned_silwood$Date)
class(cleaned_silwood$DateTime)

##### clean sunrise_sunset ####
sunrise_sunset$SunRiseDT <- as.POSIXct(sunrise_sunset$SunRiseDT,  format = "%d/%m/%Y %H:%M:%S", tz="GMT")
sunrise_sunset$SunSetDT <- as.POSIXct(sunrise_sunset$SunSetDT,  format = "%d/%m/%Y %H:%M:%S", tz="GMT")
sunrise_sunset$Date <- as.Date(sunrise_sunset$Date, '%d/%m/%Y')
class(sunrise_sunset$Date)
class(sunrise_sunset$SunRiseDT)
class(sunrise_sunset$SunSetDT)

##### change classes - Time ####
# extract from DateTime variable
cleaned_silwood$Time2 <- strftime(cleaned_silwood$DateTime, format = "%H:%M:%S", tz = "GMT")
sunrise_sunset$SunRiseTime <- strftime(sunrise_sunset$SunRiseDT, format ="%H:%M:%S", tz = "GMT")
sunrise_sunset$SunSetTime <- strftime(sunrise_sunset$SunSetDT, format ="%H:%M:%S", tz = "GMT")

# make it a time class
cleaned_silwood$Time2 <- chron(times=cleaned_silwood$Time2)
sunrise_sunset$SunRiseTime <- chron(times=sunrise_sunset$SunRiseTime)
sunrise_sunset$SunSetTime <- chron(times=sunrise_sunset$SunSetTime)

### add new column to define, day or light hits ####
head(cleaned_silwood)
i = 92

cleaned_silwood$light <- NA
for (i in 1: nrow(cleaned_silwood)){
  capturetime <- cleaned_silwood$DateTime[i]
  capturedate <- cleaned_silwood$Date[i]
  sunset <- sunrise_sunset$SunSetDT[sunrise_sunset$Date == capturedate]
  sunrise <- sunrise_sunset$SunRiseDT[sunrise_sunset$Date == capturedate]
  if (capturetime > sunrise & capturetime < sunset) { cleaned_silwood$light[i] = "1"}
  else{ cleaned_silwood$light[i] = "0"}
  print(i)
}

rm(capturedate, capturetime, i, sunrise, sunset)

write.csv(cleaned_silwood, file = "Cleaned_Silwood_Sunrise_Sunset.csv")


# this dataset should now be used for any sunrise-sunset analysis

cleaned_silwood <- read.csv("Cleaned_Silwood_Sunrise_Sunset.csv", header = T)
cleaned_silwood$Date <-as.Date(cleaned_silwood$Date, format = "%Y-%m-%d")
##### actogram #######

p1 <- ggplot(cleaned_silwood, aes(hour, Date)) + 
  geom_point(aes(colour = factor(light))) + 
  scale_color_manual(values = c("gray58", "gold2")) +
  facet_grid(. ~ Species) +
  scale_y_date(date_labels = "%Y/%B") +
  theme(legend.title = element_blank() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black")) +
          labs(x="Hour", y="Date")
  )

