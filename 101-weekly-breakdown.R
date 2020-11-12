load('rda/covid_ct.rda')
load('rda/theme_DataStache.rda')

head(covid_ct)

min(covid_ct$date)

covid_ct_weekly <- covid_ct %>%
  filter(date >= ymd(20200329)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week) %>%
  summarize(new_cases = sum(new_cases),
            new_tests = sum(new_tests),
            avg_hos = mean(current_hosp),
            new_death = sum(new_deaths))

week_start <- covid_ct %>%
  filter(date >= ymd(20200329)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week) %>%
  arrange(date) %>%
  slice_head(., n = 1) %>%
  arrange(desc(date)) %>%
  select(week, date)

covid_ct_weekly <- covid_ct_weekly %>% inner_join(week_start) %>% arrange(desc(date))

## CASES
this_week <- covid_ct_weekly$new_cases[1]
last_week <- covid_ct_weekly$new_cases[2]
change <- round(((this_week - last_week) / last_week) * 100, 1)

p_Cases <- covid_ct_weekly %>%
  ggplot(aes(x = as.factor(date), y = new_cases)) +
  geom_col() +
  ggtitle('Weekly Cases',
          subtitle = paste(change, '% From Last Week', sep = '')) +
  theme_DataStache()

## TESTS
this_week <- covid_ct_weekly$new_tests[1]
last_week <- covid_ct_weekly$new_tests[2]
change <- round(((this_week - last_week) / last_week) * 100, 1)

p_Tests <- covid_ct_weekly %>%
  ggplot(aes(x = date, y = new_tests)) +
  geom_col() +
  ggtitle('Weekly Tests',
          subtitle = paste(change, '% From Last Week', sep = '')) +
  theme_DataStache()

## HOSP
this_week <- covid_ct_weekly$avg_hos[1]
last_week <- covid_ct_weekly$avg_hos[2]
change <- round(((this_week - last_week) / last_week) * 100, 1)

p_Hosp <- covid_ct_weekly %>%
  ggplot(aes(x = date, y = avg_hos)) +
  geom_col() +
  ggtitle('Avg Weekly Hospitalizaed',
          subtitle = paste(change, '% From Last Week', sep = '')) +
  theme_DataStache()

## DEATH
this_week <- covid_ct_weekly$new_death[1]
last_week <- covid_ct_weekly$new_death[2]
change <- round(((this_week - last_week) / last_week) * 100, 1)

p_Death <- covid_ct_weekly %>%
  ggplot(aes(x = date, y = new_death)) +
  geom_col() +
  ggtitle('Weekly Deaths',
          subtitle = paste(change, '% From Last Week', sep = '')) +
  theme_DataStache()


