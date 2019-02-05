

# Daily anomalies were the difference of each daily minimum temperature from a daily ‘base’ 
# value (Caesar et al. 2006). Daily ‘base’ minimum temperatures were calculated for each climate 
# station’s minimum temperature records from 1961 to 1990 using a five-day window centered on 
# each day (Caesar et al. 2006). 

# Nov - Feb

# Finally, we used daily minimum temperature
# anomaly data from the HadGHCND station nearest the
# southwestern Idaho kestrel population to examine changes
# in winter and spring minimum temperatures from 1987 to
# 2009. Spring months were defined as March and April 

# 1) five-day window
# 2) daily means 1961 to 1990 (is this 30 years?)

library(zoo)


# 5-day moving window -----------------------------------------------------

# use 'rollmean' function
dailyTemps <- dailyTemps %>% 
  mutate(rollval = rollmean(value, k = 5, fill = NA)) 

tail(dailyTemps)


# 30-yr average (1961-1990) for each day ----------------------------------

dailyTemps <- dailyTemps %>%
  mutate(
    jday = yday(date),
    year = year(date)
  )
dailyTemps

thirtyYearSpan <- dailyTemps %>%
  filter(
    year > 1960 & year < 1991
  )
thirtyYearSpan

thirtyYearSpan <- thirtyYearSpan %>%
  group_by(jday) %>%
  mutate(avg1961_1990 = mean(rollval, na.rm = TRUE)) %>%
  select(jday, avg1961_1990) %>%
  ungroup()
thirtyYearSpan

dailyTempAnoms <- dailyTemps %>%
  left_join(., thirtyYearSpan, by = 'jday') %>%
  distinct(.) %>%
  mutate(anom = rollval - avg1961_1990)
dailyTempAnoms

dailyTempAnoms <- dailyTempAnoms %>%
  mutate(month = month(date)) %>%
  filter(month %in% c(11, 12, 1, 2))


dailyTempAnoms <- dailyTempAnoms %>%
  mutate(
    winterYear = case_when(
      month %in% c(11, 12) ~ year + 1,
      TRUE ~ year
    )
  ) 

dailyTempAnoms2 <- dailyTempAnoms %>%
  group_by(winterYear) %>%
  summarise(anom = mean(anom, na.rm = TRUE))

dailyTempAnoms2 %>%
  filter(winterYear > 1986 & winterYear < 2010) %>%
  ggplot(., aes(winterYear, anom)) +
    geom_line() +
    geom_smooth(method = 'lm')

dailyTempAnoms2
summary(lm(anom ~ winterYear, data = dailyTempAnoms2 %>% filter(winterYear > 1986 & winterYear < 2010)))
