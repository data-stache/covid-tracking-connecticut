library(tidyverse)
library(lubridate)
library(rnoaa)
library(zoo)
load("rda/ct_stations.rda")

meteo_clear_cache(force = FALSE)

ct_weather <- meteo_pull_monitors(ct_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

ct_weather <- ct_weather %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL CT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup()
# REORDER COLUMNS
ct_weather <- ct_weather[,c(1,2,5,3,6,4,7)]
save(ct_weather, file = "rda/ct_weather.rda")

