library(tidyverse)
library(broom)

load("rda/covid_ct_counties.rda")
load("rda/mobility.rda")

dat <- covid_ct_counties %>% 
  inner_join(mobility)

dat %>% select(county, date, new_cases_percap_07da)

dat %>%
  group_by(county) %>%
  summarise(ret = sum(is.na(retail_recreation)) / n(),
            groc = sum(is.na(grocery_pharmacy)) / n(),
            park = sum(is.na(parks)) / n(),
            trans = sum(is.na(transit)) / n(),
            work = sum(is.na(workplace)) / n(),
            res = sum(is.na(residential)) / n())

names(dat)

##### EXPLORATORY GRAPHS #####
dat %>%
  filter(!is.na(new_cases_percap_07da) & !is.na(residential_07da)) %>%
  ggplot(aes(x = residential_07da, y = new_cases_percap_07da, col = county)) +
  geom_point() +
  facet_wrap(. ~ county)

dat %>%
  filter(!is.na(new_cases_percap_07da) & !is.na(workplace_07da)) %>%
  ggplot(aes(x = workplace_07da, y = new_cases_percap_07da, col = county)) +
  geom_point() +
  facet_wrap(. ~ county)

dat %>%
  filter(!is.na(new_cases_percap_07da) & !is.na(retail_recreation_07da)) %>%
  ggplot(aes(x = retail_recreation_07da, y = new_cases_percap_07da, col = county)) +
  geom_point() +
  facet_wrap(. ~ county)





dat %>%
  group_by(county) %>%
  filter(!is.na(new_cases_percap_07da) & !is.na(retail_recreation_07da)) %>%
  summarise(cor = cor(new_cases_percap_07da, retail_recreation_07da))

dat %>%
  group_by(county) %>%
  filter(!is.na(new_cases_percap_07da) & !is.na(retail_recreation_07da)) %>%
  do(tidy(lm(new_cases_percap_07da ~ retail_recreation_07da, data = .), conf.int = TRUE)) %>%
  filter(!term == "(Intercept)")

