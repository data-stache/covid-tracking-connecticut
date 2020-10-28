library(tidyverse)
library(lubridate)
library(rnoaa)
library(zoo)

weather <- read.csv("data/ct_weather.csv")
stations <- weather %>%
  filter(!is.na(TAVG)) %>%
  .$STATION
stations <- unique(stations)

##### WEATHER STATION 1 CONNECTICUT #####
# PULL DATA FOR STATION
ct_weather_1 <- ghcnd(stations[1]) %>%
  filter(element == "TAVG")
# COLUMN NAME PULL
test <- names(ct_weather_1)
col_ind <- test %>%
  str_detect(.,"VALUE")
col_ind[1:3] <- TRUE
# COLUMN REPLACE
ct_weather_1 <- ct_weather_1[,col_ind] %>%
  # ONLY 2020
  filter(year == 2020) %>%
  # GATHER TABLE TO TIDY TABLE
  gather(day, avg_t, VALUE1:VALUE31) %>%
  # ARRANGE BY MONTH
  arrange(month) 

# STRIP "VALUE"
ct_weather_1$day <- str_replace(ct_weather_1$day, "VALUE", "")


ct_weather_1 <- ct_weather_1 %>%
  # DAY TO INTERGER
  mutate(day = as.integer(day),
         # CONVERT TO FARENHEIGHT
         avg_t = round(((avg_t * .1) * (9/5)) + 32),
         # ADD LEADING 0's
         month = str_pad(month, 2, pad = "0"),
         day = str_pad(day, 2, pad = "0"),
         # PASTE DATE
         date = paste(year, month, day, sep = "-")) %>%
  # FILTERS DAYS THAT DON'T EXIST
  filter(!date %in% c("2020-02-30", "2020-02-31", "2020-04-31", "2020-06-31", "2020-09-31", "2020-11-31")) %>%
  # CONVERTS TO DATE
  mutate(date = ymd(date))

# GRABS COLUMNS
ct_weather_1 <- ct_weather_1[,c(6,5)]

##### WEATHER STATION 2 #####
# PULL DATA FOR STATION
ct_weather_2 <- ghcnd(stations[1]) %>%
  filter(element == "TAVG")
# COLUMN NAME PULL
test <- names(ct_weather_2)
col_ind <- test %>%
  str_detect(.,"VALUE")
col_ind[1:3] <- TRUE
# COLUMN REPLACE
ct_weather_2 <- ct_weather_2[,col_ind] %>%
  # ONLY 2020
  filter(year == 2020) %>%
  # GATHER TABLE TO TIDY TABLE
  gather(day, avg_t, VALUE1:VALUE31) %>%
  # ARRANGE BY MONTH
  arrange(month) 

# STRIP "VALUE"
ct_weather_2$day <- str_replace(ct_weather_2$day, "VALUE", "")


ct_weather_2 <- ct_weather_2 %>%
  # DAY TO INTERGER
  mutate(day = as.integer(day),
         # CONVERT TO FARENHEIGHT
         avg_t = round(((avg_t * .1) * (9/5)) + 32),
         # ADD LEADING 0's
         month = str_pad(month, 2, pad = "0"),
         day = str_pad(day, 2, pad = "0"),
         # PASTE DATE
         date = paste(year, month, day, sep = "-")) %>%
  # FILTERS DAYS THAT DON'T EXIST
  filter(!date %in% c("2020-02-30", "2020-02-31", "2020-04-31", "2020-06-31", "2020-09-31", "2020-11-31")) %>%
  # CONVERTS TO DATE
  mutate(date = ymd(date))

# GRABS COLUMNS
ct_weather_2 <- ct_weather_2[,c(6,5)]

ct_weather <- rbind(ct_weather_1, ct_weather_2) %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(avg_t = mean(avg_t, na.rm = TRUE)) %>%
  filter(!is.na(avg_t)) %>%
  mutate(avg_t_07da = rollapply(avg_t, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup() 
save(ct_weather, file = "rda/ct_weather.rda")

