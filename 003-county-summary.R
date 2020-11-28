# LOAD DATA
load("rda/covid_ct_counties.rda")
load("rda/covid_ct_counties_zone.rda")
load("rda/covid_ct_counties_sum.rda")
load("rda/ind_tylm_date.rda")

## Pick County
cnty <- "New Haven"

library(knitr)
# DAILY LOOKUP TOTAL
covid_ct_counties %>%
  select(date, county, new_cases, new_tests, percent_pos, new_deaths, hospitalization, new_hosp) %>%
  filter(county == cnty) %>%
  filter(date %in% ind_tylm_date) %>%
  kable()

# DAILY LOOKUP PER CAPITA
covid_ct_counties %>%
  select(date, county, new_cases_percap, new_tests_percap, percent_pos, new_deaths_percap, new_hosp_percap) %>%
  filter(county == cnty) %>%
  filter(date %in% ind_tylm_date) %>%
  kable()

# NEW CASE PER CAPITA LAST 7 DAYS
merge(covid_ct_counties_sum, covid_ct_counties_zone, by = c("county", "sum_cases_percap", "percent_pos")) %>%
  filter(county == cnty) %>%
  select(county, sum_cases_percap, sum_tests_percap, percent_pos, sum_deaths_percap, sum_hosp_percap, cases_zone, percent_zone) %>%
  kable()
