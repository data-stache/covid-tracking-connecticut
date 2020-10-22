# LOAD DATA
load("rda/covid_ct_counties.rda")
load("rda/theme_DataStache.rda")

##### PLOT CHARTS
# NEW CASES
p_new_case_percap_all <- covid_ct_counties %>%
  ggplot(aes(date, new_cases_percap_07da)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_line(aes(col = county), size = .25) +
  ggtitle("Connecticut New Cases Per Capita",
          subtitle = "New Cases by County") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  scale_color_brewer(palette = "Dark2") +
  theme_DataStache() +
  facet_wrap(. ~ county, nrow = 2, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(1), rel(1), rel(1), rel(1)))) +
  gghighlight(label_key = county, use_direct_label = FALSE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  fontface = 2,
                                  nudge_y = .5,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# NEW TESTS
p_new_tests_percap_all <- covid_ct_counties %>%
  ggplot(aes(date, new_tests_percap_07da)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_line(aes(col = county), size = .25) +
  ggtitle("Connecticut New Tests Per Capita",
          subtitle = "New Tests by County") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  scale_color_brewer(palette = "Dark2") +
  theme_DataStache() +
  facet_wrap(. ~ county, nrow = 2, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(1), rel(1), rel(1), rel(1)))) +
  gghighlight(label_key = county, use_direct_label = FALSE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  fontface = 2,
                                  nudge_y = .5,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# NEW DEATHS
p_new_deaths_percap_all <- covid_ct_counties %>%
  ggplot(aes(date, new_deaths_percap_07da)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_line(aes(col = county), size = .25) +
  xlab("Date") +
  ylab("New Deaths Per 100k") +
  ggtitle("Connecticut New Deaths Per Capita",
          subtitle = "New Deaths by County") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  scale_color_brewer(palette = "Dark2") +
  theme_DataStache() +
  facet_wrap(. ~ county, nrow = 2, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(1), rel(1), rel(1), rel(1)))) +
  gghighlight(label_key = county, use_direct_label = FALSE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  fontface = 2,
                                  nudge_y = .5,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# NEW HOSPITALIZATION
p_hosp_percap_all <- covid_ct_counties %>%
  ggplot(aes(date, new_hosp_percap_07da)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_line(aes(col = county), size = .25) +
  xlab("Date") +
  ylab("New Deaths Per 100k") +
  ggtitle("Connecticut New Hospitalization",
          subtitle = "New Hospitalization by County") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_color_brewer(palette = "Dark2") +
  theme_DataStache() +
  facet_wrap(. ~ county, nrow = 2, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(1), rel(1), rel(1), rel(1)))) +
  gghighlight(label_key = county, use_direct_label = FALSE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  fontface = 2,
                                  nudge_y = .5,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# CURRENT HOSPITALIZATION
p_hosp_current <- covid_ct_counties %>%
  ggplot(aes(date, hospitalization)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_line(aes(col = county), size = .25) +
  xlab("Date") +
  ylab("New Deaths Per 100k") +
  ggtitle("Connecticut Current Hospitalization",
          subtitle = "New Hospitalization by County") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_color_brewer(palette = "Dark2") +
  theme_DataStache() +
  facet_wrap(. ~ county, nrow = 2, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(1), rel(1), rel(1), rel(1)))) +
  gghighlight(label_key = county, use_direct_label = FALSE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  fontface = 2,
                                  nudge_y = .5,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

p_width <- 6
p_height <- (9/16) * p_width

# GRID ARRANGE PLOTS
p_new_case_percap_all
ggsave("figs/CT New Cases by County.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

p_hosp_percap_all
ggsave("figs/CT Hosp by County.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

p_new_tests_percap_all
ggsave("figs/CT New Tests by County.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

p_new_deaths_percap_all
ggsave("figs/CT New Deaths by County.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

p_hosp_current
ggsave("figs/CT Current Hospitalization by County.png",
       width = p_width,
       height = p_height,
       dpi = "retina")
