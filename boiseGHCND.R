

library(rnoaa)
options(noaakey= "mMwtILsLTqlXCnQbELrXHkuKLFrCZEir") 

tmin <- ncdc(datasetid='GHCND', 
             stationid='GHCND:USW00024131', 
             datatypeid='TMIN', 
             startdate = '2018-01-01', 
             enddate = '2019-01-01',
             limit = 1000)

tmin <- tmin$data %>%
  as_tibble() %>%
  mutate(value = value/10) %>%
  mutate(date = as.Date(str_sub(date, start = 1, end = 10))) 

tmax <- ncdc(datasetid='GHCND', 
             stationid='GHCND:USW00024131', 
             datatypeid='TMAX', 
             startdate = '2018-01-01', 
             enddate = '2019-01-01',
             limit = 1000)

tmax <- tmax$data %>%
  as_tibble() %>%
  mutate(value = value/10) %>%
  mutate(date = as.Date(str_sub(date, start = 1, end = 10))) 
tmax

temps <- bind_rows(tmin, tmax)

ggplot() +
  geom_line(data = temps, aes(date, value, color = datatype), size = 1) +
  geom_vline(xintercept = as.Date('2018-08-10'), linetype = 2) + # record day
  ggtitle('2018 temperatures')

# record hot 110 F day in Boise
tmax %>%
  filter(date == '2018-08-10')
