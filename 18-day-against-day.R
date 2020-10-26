# LOAD DATA
load("rda/covid_ct.rda")
load("rda/theme_DataStache.rda")
head(covid_ct)

##### BOX PLOT REFERENCES #####
# Box Plot of New Cases Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Thursday", "Wednesday", "Tuesday", "Monday", "Sunday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = new_cases)) +
  geom_boxplot(fill = "light blue", size = .25, outlier.shape = NA) +
  geom_point(color = "Dark blue", size = .5) +
  ggtitle("Does Day Of The Week Impact New Case Count?",
          subtitle = "Daily New Cases Reported Sunday - Thursday Since Connecticut Stopped Weekend Reporting") +
  theme_DataStache() +
  coord_flip()

p_width <- 6
p_height <- (9/16) * p_width

ggsave("figs/Connecticut New Report Box Plot.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

# Box Plot of New Deaths Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = new_deaths)) +
  geom_boxplot() +
  geom_point() +
  ggtitle("Connecticut New Deaths Box Plot") +
  theme_DataStache()

# Box Plot of New Hospitalization Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = new_hosp)) +
  geom_boxplot() +
  geom_point() +
  ggtitle("Connecticut New Hospitalization Box Plot") +
  theme_DataStache()

# Box Plot of New Testing Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = new_tests)) +
  geom_boxplot() +
  geom_point() +
  ggtitle("Connecticut New Testing Box Plot") +
  theme_DataStache()

# Box Plot of Percent Positive Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = percent_pos)) +
  geom_boxplot() +
  geom_point() +
  ggtitle("Connecticut Percent Positive Box Plot") +
  theme_DataStache()


##### DAY AGAINST DAY #####
# PERCENTAGE OF CASES REPORTED ON WHAT DAYS OF THE WEEK
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>% 
  group_by(week) %>%
  mutate(weeks = max(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ungroup() %>%
  group_by(day) %>%
  summarise(case_sum = sum(new_cases)) %>%
  mutate(per_case = case_sum / sum(case_sum))

# New Cases Compared to this Day of the week
Day <- covid_ct$day[1]
# !! OR  !!
Day <- "Sunday"

covid_ct %>%
  filter(day == Day) %>%
  ggplot(aes(x = as.factor(date), y = new_cases)) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_line(group=1, color = "dark blue", size = .3) +
  geom_point(color = "dark blue", size = .3) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_y_continuous(breaks = seq(0, 5000, 200)) +
  ggtitle("Does Day of the Week Impact New Case Counts?",
          subtitle = paste("Connecticut New Cases Compared to other", Day)) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90))

p_width <- 6
p_height <- (9/16) * p_width 

ggsave("figs/Day of Week Compare.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

ct_total_infected

covid_ct %>%
  filter(date >= ymd(20200701)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>%
  group_by(day) %>%
  summarize(cases_tot = sum(new_cases),
            cases_per = sum(new_cases) / sum(.$new_cases))
            
