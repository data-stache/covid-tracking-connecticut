library(broom)
library(tidyverse)
library(caret)
library(lubridate)
library(zoo)
library(tidylog)

load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
load("rda/covid_ct.rda")
load("rda/ct_weather.rda")

head(covid_ct)
head(ct_weather)

tdy_date <- covid_ct$date[1]

# LAST UPDATE
max(ct_weather$date[!is.na(ct_weather$tavg)])

# BUILD DATA SET
dat <- covid_ct %>%
  filter(date >= ymd(20200415)) %>%
  select(date, new_cases) %>%
  left_join(ct_weather) %>%
  select(1:3) %>%
  filter(!is.na(tavg)) %>%
  mutate(cases = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         temp = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup() %>%
  select(date, cases, temp)

dat %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_hline(yintercept = 0, size = .25 , col = "grey40") +
  geom_point(size = .5, alpha = .5) +
  geom_smooth(method = 'loess') +
  ggtitle('Impact of 7 Day Temperature Average on New Covid Cases in Connecticut') +
  labs(caption = 'Created by Andrew F. Griffin\nData: NOAA and data.ct.gov') +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  coord_cartesian(expand = FALSE, xlim = c(min(dat$temp) * .99, max(dat$temp) * 1.01)) +
  theme_DataStache() +
  theme(axis.title = element_text(size = rel(.8)))

p_width <- 9
p_height <- (9/16) * p_width
ggsave(paste("figs/ct-weather-infection-regression-", tdy_date, ".png", sep = ''),
       width = p_width,
       height = p_height,
       dpi = "retina")
