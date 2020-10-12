# LOAD DATA
load("rda/covid_ct.rda")
load("rda/covid_ct_towns.rda")
load("rda/ind_tylm_date.rda")
load("rda/town_growth_top10.rda")
load("rda/county_growth_ord.rda")
load("rda/covid_ct_sum.rda")
options(digits = 3)

# DAILY LOOKUP TOTAL
covid_ct %>%
  select(date, new_cases, new_tests, percent_pos, new_deaths, current_hosp, new_hosp) %>%
  filter(date %in% ind_tylm_date)

# WEEKLY SUMS LAST 2 WEEKS
covid_ct_sum

county_growth_ord

town_growth_top10
  
