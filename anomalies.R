
# Daily anomalies were the difference of each daily minimum temperature from a daily ‘base’ 
# value (Caesar et al. 2006). Daily ‘base’ minimum temperatures were calculated for each climate 
# station’s minimum temperature records from 1961 to 1990 using a five-day window centered on 
# each day (Caesar et al. 2006). 

# Winter = Nov-Feb
# Spring = Mar-Apr

# Finally, we used daily minimum temperature
# anomaly data from the HadGHCND station nearest the
# southwestern Idaho kestrel population to examine changes
# in winter and spring minimum temperatures from 1987 to
# 2009. 

# 1) five-day window
# 2) daily means 1961 to 1990 (is this 30 years?)
# 3) calculate daily anomalies
# 4) average winter anomalies and spring anomalies

library(tidyverse)
library(lubridate)
library(zoo)

dailyTemps <- read_csv(here('data/tmin1959-2018.csv'))

# 5-day moving window -----------------------------------------------------

# use 'rollmean' function to calculate moving average
dailyTemps <- dailyTemps %>% 
  mutate(rollval = rollmean(value, k = 5, fill = NA)) 

# 30-yr average (1961-1990) for each day ----------------------------------

# add year and julian date variables
dailyTemps <- dailyTemps %>%
  mutate(
    jday = yday(date),
    year = year(date)
  )
dailyTemps

# get these calculating 30-yr averages
thirtyYearSpan <- dailyTemps %>%
  filter(
    year > 1960 & year < 1991 # 1961-1990
  )
thirtyYearSpan

# average for each julian day, 1961-1990
thirtyYearAvg <- thirtyYearSpan %>%
  group_by(jday) %>%
  mutate(avg1961_1990 = mean(rollval, na.rm = TRUE)) %>%
  select(jday, avg1961_1990) %>%
  ungroup()
thirtyYearAvg

# difference of daily rolling average to calculate daily anomaly
dailyTempAnoms <- dailyTemps %>%
  left_join(., thirtyYearAvg, by = 'jday') %>%
  distinct(.) %>%
  mutate(anom = rollval - avg1961_1990)
dailyTempAnoms


# winter anomalies --------------------------------------------------------

# select november, december, january, february
dailyWinterAnoms <- dailyTempAnoms %>%
  mutate(month = month(date)) %>%
  filter(month %in% c(11, 12, 1, 2))
dailyWinterAnoms

# associate november and december with the next year...so Nov & Dec 2015 is included in winter 2016 average
dailyWinterAnoms <- dailyWinterAnoms %>%
  mutate(
    winterYear = case_when(
      month %in% c(11, 12) ~ year + 1,
      TRUE ~ year
    )
  )
dailyWinterAnoms 

# average daily anomalies over winter months
dailyWinterAnoms <- dailyWinterAnoms %>%
  group_by(winterYear) %>%
  filter(winterYear > 1986 & winterYear < 2010) %>% # 1987-2009
  summarise(anom = mean(anom))
dailyWinterAnoms


# spring anomalies --------------------------------------------------------

dailySpringAnoms <- dailyTempAnoms %>%
  mutate(month = month(date)) %>%
  filter(month %in% c(3, 4))
dailySpringAnoms

# average daily anomalies over spring months
dailySpringAnoms <- dailySpringAnoms %>%
  group_by(year) %>%
  filter(year > 1986 & year < 2010) %>% # 1987-2009
  summarise(anom = mean(anom))
dailySpringAnoms


# plot data ---------------------------------------------------------------

# winter
dailyWinterAnoms %>%
  ggplot(., aes(winterYear, anom)) +
    geom_line() +
    geom_smooth(method = 'lm')

# spring
dailySpringAnoms %>%
  ggplot(., aes(year, anom)) +
  geom_line() +
  geom_smooth(method = 'lm')


# analyze data ------------------------------------------------------------

# winter
# from Heath et al. 2012 (beta = 0.08, significant)
summary(lm(anom ~ winterYear, data = dailyWinterAnoms)) # beta = 0.1321, significant

# winter
# from Heath et al. 2012 (beta = -0.04, not significant)
summary(lm(anom ~ year, data = dailySpringAnoms)) # beta = 0.01535, not significant

