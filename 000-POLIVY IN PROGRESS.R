library(lubridate)
library(tidyverse)
library(tidylog)

load('rda/policy.rda')
load("rda/covid_ct.rda")
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
options(scipen = 999)

CT_POP <- sum(930000, 890000, 190000, 170000, 860000, 270000, 150000, 120000)

dat <- covid_ct %>%
  left_join(policy) %>%
  select(date, new_cases, new_cases_07da, stringency_index, government_index, containment_index, economic_index, mean_index)

dat %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = (new_cases / CT_POP) * 1000)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="blue", alpha = .2, size = .1) +
  geom_line(aes(y = (new_cases_07da / CT_POP) *1000), size = .35, col="blue") +
  geom_line(aes(y = mean_index * 1), size = .7, col="red4") +
  scale_y_continuous(sec.axis = sec_axis(~./ 1, name = "Stringency")) +
  ggtitle('New Cases') +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))


dat %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = (new_cases / CT_POP) * 10000)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="blue", alpha = .2, size = .1) +
  geom_line(aes(y = (new_cases_07da / CT_POP) * 10000), size = .35, col="blue") +
  geom_line(aes(y = mean_index), size = .7, col="red4") +
#  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Stringency")) +
  ggtitle('New Cases') +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
#  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, ifelse(max(covid_ct$new_cases_07da, na.rm = TRUE) * 1.1 >= 100,
#                                                                  max(covid_ct$new_cases_07da, na.rm = TRUE) * 1.1, 100))) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))


p_ALL_states_cases_policy_by_case <- policy %>%
  mutate(state_name = factor(state_name, levels = case_order)) %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .2, size = .1, fill = "darkblue") +
  geom_line(aes(x = date, y = new_cases_percap_07da), color = "darkblue", size = .25) +
  geom_line(aes(y = mean_index * 2), color = "red4", size = .5) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Stringency")) +
  ggtitle("New Cases of Covid 19 per 100k People vs State Mitigation Efforts",
          subtitle = "Ordered from Most Stringent to Least Stringent Covid Mitigation Policy") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0, max(covid$new_cases_percap_07da) * 1.1)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state_name,
             strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.5),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

