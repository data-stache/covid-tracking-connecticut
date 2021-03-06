# LOAD DATE
load("rda/covid_ct.rda")

# COVID CASES TOP 10
covid_ct %>%
  arrange(desc(new_cases)) %>% slice_head(n=10) %>% select(date, day, new_cases)

# COVID CASES TOP 10
covid_ct %>%
  arrange(desc(new_cases_07da)) %>% slice_head(n=10) %>% select(date, day, new_cases_07da)

# CASES THIS HIGH
covid_ct %>%
  filter(new_cases >= covid_ct$new_cases[1]) %>% select(date, new_cases)

# 7 DAY AVG CASES THIS HIGH
covid_ct %>%
  filter(new_cases_07da >= covid_ct$new_cases_07da[1]) %>% select(date, new_cases_07da)

# DAILY DEATHS THIS HIGH
covid_ct %>%
  filter(new_deaths >= covid_ct$new_deaths[1]) %>% select(date, new_deaths)

# 7 DAY AVG DEATHS THIS HIGH
covid_ct %>%
  filter(new_deaths_07da >= covid_ct$new_deaths_07da[1]) %>% select(date, new_deaths_07da)

# CURRENT HOSP THIS HIGH
covid_ct %>%
  filter(current_hosp >= covid_ct$current_hosp[1]) %>% select(date, current_hosp)

# NEW HOSP THIS HIGH
covid_ct %>%
  filter(new_hosp >= covid_ct$new_hosp[1]) %>% select(date, new_hosp)

# 7 DAY AVG NEW HOSP THIS HIGH
covid_ct %>%
  filter(new_hosp_07da >= covid_ct$new_hosp_07da[1]) %>% select(date, new_hosp_07da)

# % POSITIVE ABOVE ##
p <- .0312
covid_ct %>%
  filter(percent_pos >= p) %>% select(date, percent_pos)
