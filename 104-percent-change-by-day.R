load('rda/theme_DataStache.rda')

d <- 'Monday'

covid_ct %>%
  filter(date >= ymd(20200701)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, day) %>%
  summarize(cases = sum(new_cases),
            tests = sum(new_tests),
            deaths = sum(new_deaths),
            hosp = mean(current_hosp, na.rm = TRUE)) %>%
  gather(metric, total, cases:hosp) %>%
  ungroup() %>%
  filter(day == d) %>%
  group_by(metric) %>%
  arrange(desc(week)) %>%
  mutate(change = rollapply(total, width=2, FUN=function(x) (x[1] - x[2]) / x[2], fill = NA, align="left")) %>%
  slice_head(n = 4) %>%
  kable()

covid_ct %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, day) %>%
  summarize(cases = sum(new_cases),
            tests = sum(new_tests),
            deaths = sum(new_deaths),
            hosp = mean(current_hosp, na.rm = TRUE)) %>%
  gather(metric, total, cases:hosp) %>%
  ungroup() %>%
  filter(day == d & !metric == 'deaths') %>%
  group_by(metric) %>%
  arrange(desc(week)) %>%
  mutate(change = rollapply(total, width=2, FUN=function(x) (x[1] - x[2]) / x[2] * 100, fill = NA, align="left"),
         week = ymd(20200105) + weeks(week-2),
         metric = factor(metric, levels = c('cases', 'tests', 'hosp'),
                         labels = c('Weekly % Change: Newly Reported Cases', 'Weekly % Change: New Tests', 'Weekly % Change: Current Hospitalization'))) %>%
  ungroup() %>%
  ggplot() +
  geom_hline(yintercept = 0, col = 'grey60') +
  geom_line(aes(x = week, y = change, col = metric), size = .4) +
  geom_point(aes(x = week, y = change, col = metric), size = .5) +
  theme_DataStache() +
  facet_wrap(. ~ metric, nrow = 1, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(1), rel(1), rel(1), rel(1)))) +
  ggtitle(paste('Percent Change in', d, 'Reporting')) +
  labs(caption = 'Created by Andrew F. Griffin')

p_width <- 6
p_height <- (9/16) * p_width 

ggsave('figs/percent-change-reporting.png',
       width = p_width,
       height = p_height,
       dpi = 'retina')
