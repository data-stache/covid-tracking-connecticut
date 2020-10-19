# LOAD DATA

load("rda/covid_ct")
load("rda/theme_DataStache.rda")
head(covid_ct)


# New Cases Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>% 
  group_by(week) %>%
  mutate(weeks = max(date)) %>%
  filter(date >= ymd(20200901)) %>%
  ggplot(aes(x = day, y = new_cases, group = weeks)) +
  geom_col(aes(fill = factor(weeks)), position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(fill = "Week Ending") +
  theme_DataStache() +
  theme(legend.position = "right")

# New Cases Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>% 
  group_by(week) %>%
  mutate(weeks = max(date)) %>%
  filter(date >= ymd(20200901)) %>%
  ggplot(aes(x = day, y = new_cases, group = weeks)) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_col(aes(fill = factor(weeks)), position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(fill = "Week Ending") +
  theme_DataStache() +
  ggtitle("Connecticut New Cases By Day of the Week") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  theme(legend.position = "right")


# New Cases Compared to this Day of the week
Day <- covid_ct$day[1]
covid_ct %>%
  filter(day == day[1]) %>%
  filter(date >= ymd(20200901)) %>%
  ggplot(aes(x = as.factor(date), y = new_cases)) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_line(group=1, color = "dark blue", size = .3) +
  geom_point(color = "dark blue") +
  coord_cartesian(ylim = c(0, NA)) +
  scale_y_continuous(breaks = seq(0,1000,200)) +
  ggtitle(paste("Connecticut New Cases Compared to other", Day)) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  theme_DataStache()


