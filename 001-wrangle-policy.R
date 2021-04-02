# CLEAR ENVIRONS
rm(list=ls())

library(tidyverse)
library(lubridate)
library(tidylog)

# COVID DATA
url <- "https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv"
dest_file <- "data/usa-covid-policy-by-state.csv"
download.file(url, destfile = dest_file)

policy <- read.csv('data/usa-covid-policy-by-state.csv') %>%
  mutate(Date = ymd(Date)) %>%
  arrange(desc(Date)) %>%
  select(RegionName, Date,
         StringencyIndexForDisplay, GovernmentResponseIndexForDisplay, ContainmentHealthIndexForDisplay, EconomicSupportIndexForDisplay) %>%
  rename(state = RegionName,
         date = Date,
         stringency_index = StringencyIndexForDisplay,
         government_index = GovernmentResponseIndexForDisplay,
         containment_index = ContainmentHealthIndexForDisplay,
         economic_index = EconomicSupportIndexForDisplay) %>%
  mutate(mean_index = (stringency_index + government_index + containment_index + economic_index) / 4,
         state = str_replace(state, 'US_', ''))

save(policy, file = 'rda/policy.rda')
rm(list=ls())