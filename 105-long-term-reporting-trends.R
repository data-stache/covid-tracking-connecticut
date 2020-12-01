library(dplyr)
library(lubridate)
load("rda/covid_ct.rda")
load("rda/theme_DataStache.rda")
options(scipen = 999)

head(covid_ct)

dat <- covid_ct %>%
  #filter(date >= ymd(20200701)) %>%
  filter(date >= ymd(20200401)) %>%
  mutate(month = month(date, label = TRUE),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(month, day) %>%
  summarize(cases = sum(new_cases),
            death = sum(new_deaths),
            tests = sum(new_tests)) %>%
  group_by(month) %>%
  mutate(cases_per = round(cases / sum(cases) * 100),
         death_per = round(death / sum(death) * 100),
         tests_per = round(tests / sum(tests) * 100))

# NEW CASES
dat %>%
  mutate(cases_per = paste(cases_per,"%", sep = ""),
         day = recode(day, 'Sunday' = 'Sn',
                           'Monday' = 'M',
                           'Tuesday' = 'Tu',
                           'Wednesday' = 'W',
                           'Thursday' = 'Th',
                           'Friday' = 'F',
                           'Saturday' = 'St')) %>%
  ggplot(aes(x = month, y = cases, fill = day, label = cases_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = .5, vjust = -.5) +
  geom_text(aes(y=0 , label = day), hjust = .5, vjust = -.5, position = position_dodge(.9), size = 1, fontface = 'bold') +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE, ylim = c(0, max(dat$cases) * 1.1)) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('New Covid-19 Cases Reported by Day of the Week',
          subtitle = 'Long Term Trends in Connecticut Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data data.ct.gov',
       fill = 'Day of the Week')

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/ct-cases-day-of-the-week.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

# NEW TESTS
dat %>%
  filter(!month %in% c('Apr', 'May')) %>%
  mutate(tests_per = paste(tests_per,"%", sep = ""),
         day = recode(day, 'Sunday' = 'Sn',
                      'Monday' = 'M',
                      'Tuesday' = 'Tu',
                      'Wednesday' = 'W',
                      'Thursday' = 'Th',
                      'Friday' = 'F',
                      'Saturday' = 'St')) %>%
  ggplot(aes(x = month, y = tests, fill = day, label = tests_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = .5, vjust = -.5) +
  geom_text(aes(y=0 , label = day), hjust = .5, vjust = -.5, position = position_dodge(.9), size = 1, fontface = 'bold') +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE, ylim = c(0, max(dat$tests, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('New Covid-19 Tests Reported by Day of the Week',
          subtitle = 'Long Term Trends in Connecticut Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data data.ct.gov',
       fill = 'Day of the Week')

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/ct-tests-day-of-the-week.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

# NEW DEATHS
dat %>%
  mutate(death_per = paste(death_per,"%", sep = ""),
         day = recode(day, 'Sunday' = 'Sn',
                      'Monday' = 'M',
                      'Tuesday' = 'Tu',
                      'Wednesday' = 'W',
                      'Thursday' = 'Th',
                      'Friday' = 'F',
                      'Saturday' = 'St')) %>%
  ggplot(aes(x = month, y = death, fill = day, label = death_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = .5, vjust = -.5) +
  geom_text(aes(y=0 , label = day), hjust = .5, vjust = -.5, position = position_dodge(.9), size = 1, fontface = 'bold') +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE,  ylim = c(0, max(dat$death) * 1.1)) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('New Covid-19 Deaths Reported by Day of the Week',
          subtitle = 'Long Term Trends in Connecticut Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data data.ct.gov',
       fill = 'Day of the Week')

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/ct-deaths-day-of-the-week.png",
       width = p_width,
       height = p_height,
       dpi = "retina")