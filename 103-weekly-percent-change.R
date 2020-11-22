covid_ct %>%
  mutate(week = epiweek(date)) %>%
  group_by(week) %>%
  summarize(cases = sum(new_cases),
            tests = sum(new_tests),
            deaths = sum(new_deaths),
            hosp = mean(current_hosp, na.rm = TRUE)) %>%
  gather(metric, total, cases:hosp) %>%
  ungroup() %>%
  group_by(metric) %>%
  arrange(desc(week)) %>%
  mutate(change = rollapply(total, width=2, FUN=function(x) (x[1] - x[2]) / x[2], fill = NA, align="left")) %>%
  slice_head(n = 4) %>%
  kable()

covid_ct %>%
  mutate(week = epiweek(date)) %>%
  group_by(week) %>%
  summarize(cases = sum(new_cases),
            tests = sum(new_tests),
            deaths = sum(new_deaths),
            hosp = mean(current_hosp, na.rm = TRUE)) %>%
  gather(metric, total, cases:hosp) %>%
  ungroup() %>%
  group_by(metric) %>%
  arrange(desc(week)) %>%
  mutate(change = rollapply(total, width=2, FUN=function(x) (x[1] - x[2]) / x[2], fill = NA, align="left")) %>%
  ungroup() %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = week, y = change, col = metric)) +
  geom_point(aes(x = week, y = change, col = metric)) +
  facet_wrap(. ~ metric, nrow = 1, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(1), rel(1), rel(1), rel(1)))) +
  gghighlight(label_key = metric, use_direct_label = TRUE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  fontface = 2,
                                  nudge_y = .2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))
