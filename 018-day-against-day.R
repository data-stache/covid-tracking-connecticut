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
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_boxplot(fill = "light blue", size = .25, outlier.shape = NA) +
  geom_jitter(color = "Dark blue", size = .5, width = .15, alpha = .5) +
  ggtitle("Day Of The Week VS New Case Count",
          subtitle = "Since July 1, 2020") +
  labs(caption = 'Created by Andrew F. Griffin') +
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
         day = factor(day, levels = c("Thursday", "Wednesday", "Tuesday", "Monday", "Sunday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = new_deaths)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_boxplot(fill = "light blue", size = .25, outlier.shape = NA) +
  geom_jitter(color = "Dark blue", size = .5, width = .15, alpha = .5) +
  ggtitle("Day Of The Week VS Death Count",
          subtitle = "Since July 1, 2020") +
  labs(caption = 'Created by Andrew F. Griffin') +
  theme_DataStache() +
  coord_flip()

ggsave("figs/Connecticut New Death Report Box Plot.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

# Box Plot of New Hospitalization Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Thursday", "Wednesday", "Tuesday", "Monday", "Sunday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = new_hosp)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_boxplot(fill = "light blue", size = .25, outlier.shape = NA) +
  geom_jitter(color = "Dark blue", size = .5, width = .15, alpha = .5) +
  ggtitle("Day Of The Week VS New Hospitalization Reported",
          subtitle = "Since July 1, 2020") +
  labs(caption = 'Created by Andrew F. Griffin') +
  theme_DataStache() +
  coord_flip()

ggsave("figs/Connecticut Hospitalization Report Box Plot.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

# Box Plot of New Testing Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Thursday", "Wednesday", "Tuesday", "Monday", "Sunday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = new_tests)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_boxplot(fill = "light blue", size = .25, outlier.shape = NA) +
  geom_jitter(color = "Dark blue", size = .5, width = .15, alpha = .5) +
  ggtitle("Day Of The Week VS New Testing Reported",
          subtitle = "Since July 1, 2020") +
  labs(caption = 'Created by Andrew F. Griffin') +
  theme_DataStache() +
  coord_flip()

ggsave("figs/Connecticut New Testing Report Box Plot.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

# Box Plot of Percent Positive Day of Week over Time
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Thursday", "Wednesday", "Tuesday", "Monday", "Sunday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = day, y = percent_pos)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_boxplot(fill = "light blue", size = .25, outlier.shape = NA) +
  geom_jitter(color = "Dark blue", size = .5, width = .15, alpha = .5) +
  ggtitle("Day Of The Week VS Percent Share Positive Tests Reported",
          subtitle = "Since July 1, 2020") +
  labs(caption = 'Created by Andrew F. Griffin') +
  theme_DataStache() +
  coord_flip()

ggsave("figs/Connecticut percent Positive Report Box Plot.png",
       width = p_width,
       height = p_height,
       dpi = "retina")


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

covid_ct %>%
  filter(date >= ymd(20200701)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>%
  group_by(day) %>%
  summarize(cases_tot = sum(new_cases),
            cases_per = sum(new_cases) / sum(.$new_cases))
            
