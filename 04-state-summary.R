# LOAD DATA
load("rda/covid_ct.rda")
load("rda/ind_tylm_date.rda")
load("rda/town_growth_top10.rda")
load("rda/covid_ct_counties_sum.rda")
load("rda/covid_ct_counties_zone.rda")
load("rda/covid_ct_sum.rda")
options(digits = 3)

# DAILY LOOKUP TOTAL
covid_ct %>%
  select(date, new_cases, new_tests, percent_pos, new_deaths, current_hosp, new_hosp) %>%
  filter(date %in% ind_tylm_date)

# WEEKLY SUMS LAST 2 WEEKS
covid_ct_sum

merge(covid_ct_counties_sum, covid_ct_counties_zone, by = c("county", "sum_cases_percap", "percent_pos")) %>%
  select(county, sum_cases_percap, sum_tests_percap, percent_pos, sum_deaths_percap, sum_hosp_percap, cases_zone, percent_zone) %>%
  arrange(desc(sum_cases_percap))

town_growth_top10
  
