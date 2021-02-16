{
  library(lubridate)
  library(knitr)
  library(zoo)
  library(stringr)
  library(tidyverse)
  library(rvest)
  library(tidylog)
  library(ggplot2)
}
options(scipen = 999)

RUN_DATE <- Sys.Date()
CT_POP <- sum(930000, 890000, 190000, 170000, 860000, 270000, 150000, 120000)

vaccinations <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv')

vaccinations <- vaccinations %>%
  filter(location == 'Connecticut') %>%
  select(date, state = location, total_vaccinations, people_vaccinated, people_fully_vaccinated, share_doses_used) %>%
  mutate(date = ymd(date)) %>%
  mutate_at(c('total_vaccinations', 'people_vaccinated', 'people_fully_vaccinated', 'share_doses_used'), na.approx) %>%
  arrange(desc(date)) %>%
  # New Vaccinations
  mutate(new_vaccinations = total_vaccinations - lag(total_vaccinations, n = 1, order_by = date),
         new_people_vaccinated = people_vaccinated - lag(people_vaccinated, n = 1, order_by = date),
         new_fully_vaccinated = people_fully_vaccinated - lag(people_fully_vaccinated, n = 1, order_by = date),
         # Raw Rolling 7 Day Average
         new_vaccinations_07da = rollapply(new_vaccinations, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_people_vaccinated_07da = rollapply(new_people_vaccinated, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_fully_vaccinated_07da = rollapply(new_fully_vaccinated, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         # Per Capita Data
         vaccinations_per_cap = (total_vaccinations / CT_POP) * 100000,
         people_vacinated_per_cap = (people_vaccinated / CT_POP) * 100000,
         fully_vaccinated_per_cap = (people_fully_vaccinated / CT_POP) * 100000,
         # Per Capita Daily New Vaccinations
         new_vaccinations_per_cap = vaccinations_per_cap - lag(vaccinations_per_cap, n = 1, order_by = date),
         new_people_vaccinated_per_cap = people_vacinated_per_cap - lag(people_vacinated_per_cap, n = 1, order_by = date),
         new_fully_vaccinated_per_cap = fully_vaccinated_per_cap - lag(fully_vaccinated_per_cap, n = 1, order_by = date),
         # Per Capita New Vaccinations Rolling 7 Day
         new_vaccinations_per_cap_07da = rollapply(new_vaccinations_per_cap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_people_vaccinated_per_cap_07da = rollapply(new_people_vaccinated_per_cap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_fully_vaccinated_per_cap_07da = rollapply(new_fully_vaccinated_per_cap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         # Share Populations
         share_used_doses = share_doses_used,
         share_pop_vaccinated = people_vaccinated / CT_POP,
         share_pop_fully_vaccinated = people_fully_vaccinated / CT_POP) %>%
  select(-share_doses_used)

head(vaccinations)

save(vaccinations, file = 'rda/vaccinations.rda')

vaccinations %>%
  filter(date == max(date)) %>%
  select(date, share_used_doses, share_pop_vaccinated, share_pop_fully_vaccinated, new_vaccinations_07da, new_people_vaccinated_07da, new_fully_vaccinated_07da) %>%
  kable()


