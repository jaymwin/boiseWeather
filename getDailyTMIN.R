
library(tidyverse)
library(here)
library(rnoaa)
library(lubridate)
library(scales)
library(viridis)

here <- here::here

# need this to use rnoaa
options(noaakey= "mMwtILsLTqlXCnQbELrXHkuKLFrCZEir") 

# there are download restrictions from NOAA; need to download by year
years <- seq(from = 1959, to = 2018, by = 1)

# create lists of start and end dates for each year
years <- years %>%
  as_tibble() %>%
  rename(year = value) %>%
  mutate(
    startDate = paste(year, '01-01', sep = '-'),
    endDate = paste(year, '12-31', sep = '-')
  )
years

startDates <- years %>%
  pull(startDate)
startDates

endDates <- years %>%
  pull(endDate)
endDates

# to store output of for loop
tempsList <- list()

for (i in seq_along(startDates)) {

  # get daily minimum temp from Boise airport
  tmin <- ncdc(datasetid='GHCND', 
               stationid='GHCND:USW00024131', 
               datatypeid='TMIN', 
               startdate = startDates[i], 
               enddate = endDates[i],
               limit = 1000)
  
  # turn into tibble, convert 1/10s of C to C
  tmin <- tmin$data %>%
    as_tibble() %>%
    mutate(value = value/10) %>%
    mutate(date = as.Date(str_sub(date, start = 1, end = 10))) 

  # store each year of data here
  tempsList[[i]] <- tmin

}

# combine all temperatures and write to csv
dailyTemps <- tempsList %>%
  bind_rows() 

dailyTemps %>%
  write_csv(here('data/tmin1959-2018.csv'))

# plot it
dailyTemps %>%
  mutate(jday = yday(date)) %>%
  mutate(year = year(date)) %>%
  group_by(jday) %>%
  summarise(meanTMIN = mean(value)) %>%
    ggplot(., aes(jday, meanTMIN)) +
      geom_line()

dailyTemps %>%
  mutate(jday = yday(date)) %>%
  mutate(year = year(date)) %>%
    ggplot(., aes(jday, value, color = year)) +
      geom_line() +
      scale_color_viridis()

ggsave(here('plots/allYears.png'), units = 'in', height = 3, width = 6)
