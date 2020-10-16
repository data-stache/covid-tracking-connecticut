# LOAD DATE
load("rda/covid_ct_counties.rda")

# PICK A COUNTY
cnty <- "Windham"
covid_ct_cnty_lkup <- covid_ct_counties %>% filter(county == cnty)

# CASES THIS HIGH
covid_ct_cnty_lkup %>%
  filter(new_cases >= covid_ct_cnty_lkup$new_cases[1]) %>% select(date, new_cases)

# 7 DAY AVG CASES THIS HIGH
covid_ct_cnty_lkup %>%
  filter(new_cases_07da >= covid_ct_cnty_lkup$new_cases_07da[1]) %>% select(date, new_cases_07da)

# DAILY DEATHS THIS HIGH
covid_ct_cnty_lkup %>%
  filter(new_deaths >= covid_ct_cnty_lkup$new_deaths[1]) %>% select(date, new_deaths)

# 7 DAY AVG DEATHS THIS HIGH
covid_ct_cnty_lkup %>%
  filter(new_deaths_07da >= covid_ct_cnty_lkup$new_deaths_07da[1]) %>% select(date, new_deaths_07da)

# CURRENT HOSP THIS HIGH
covid_ct_cnty_lkup %>%
  filter(current_hosp >= covid_ct_cnty_lkup$current_hosp[1]) %>% select(date, current_hosp)

# NEW HOSP THIS HIGH
covid_ct_cnty_lkup %>%
  filter(new_hosp >= covid_ct_cnty_lkup$new_hosp[1]) %>% select(date, new_hosp)

# 7 DAY AVG NEW HOSP THIS HIGH
covid_ct_cnty_lkup %>%
  filter(new_hosp_07da >= covid_ct_cnty_lkup$new_hosp_07da[1]) %>% select(date, new_hosp_07da)

# % POSITIVE ABOVE ##
p <- .04
covid_ct_cnty_lkup %>%
  filter(percent_pos >= p) %>% select(date, percent_pos)
