##### STATE GRAPHS #####
# LOAD DATA
load("rda/covid_ct.rda")
load("rda/theme_DataStache.rda")

tdy_date <- covid_ct$date[1]

##### PLOT CHARTS
# NEW CASES
p_new_case <- covid_ct %>%
  ggplot(aes(date, new_cases)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  # INSERT PHASE 3 REOPEN MARKER
  #geom_vline(xintercept = ymd(20201008), size = .15, color = "grey40") +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_07da), size = .25, col="blue") +
  ggtitle("Connecticut New Cases Total",
          subtitle = "New Cases Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, max(covid_ct$new_cases_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# NEW TESTS        
p_new_test <- covid_ct %>%
  ggplot(aes(date, new_tests)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_07da), size = .25, col="dark green") +
  ggtitle("Connecticut New Tests Total",
          subtitle = "Tests Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, max(covid_ct$new_tests_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# NEW DEATHS
p_new_deaths <- covid_ct %>%
  ggplot(aes(date, new_deaths)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_deaths_07da), size = .25, col="dark red") +
  ggtitle("Connecticut New Deaths Total",
          subtitle = "Deaths Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, max(covid_ct$new_deaths_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# NEW HOSPITALIZATION
p_new_hosp <- covid_ct %>%
  ggplot(aes(date, new_hosp)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="yellow4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_hosp_07da), size = .25, col="yellow4") +
  ggtitle(paste("Connecticut New Hospitalization"),
          subtitle = "Newly Hospitalized Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(min(covid_ct$new_hosp_07da, na.rm = TRUE) * 1.1, max(covid_ct$new_hosp_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# CURRENT HOSPITALIZATION
p_hosp <- covid_ct %>%
  ggplot(aes(date, current_hosp)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="orange4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = cur_hosp_07da), size = .25, col="orange4") +
  ggtitle(paste("Connecticut New Hospitalization"),
          subtitle = "Currently Hospitalized With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, max(covid_ct$cur_hosp_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# PERCENT POSITIVE
p_pos <- covid_ct %>%
  ggplot(aes(date, percent_pos)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="orange4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = percent_pos_07da), size = .25, col="orange4") +
  ggtitle(paste("Connecticut Percent Positive"),
          subtitle = "Percent Share Positive With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) + 
  coord_cartesian(ylim = c(0, max(covid_ct$percent_pos_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

library(gridExtra)
# GRID ARRANGE PLOTS
grid.arrange(p_new_case, p_new_test, p_hosp, p_new_deaths, p_pos, p_new_hosp, nrow = 2)

p_width <- 6
p_height <- (9/16) * p_width 

P <- arrangeGrob(p_new_case, p_new_test, p_hosp, p_new_deaths, p_pos, p_new_hosp, nrow = 2)
ggsave(paste("figs/state-daily-summary-", tdy_date, ".png", sep = ""),
       P,
       width = p_width,
       height = p_height,
       dpi = "retina")


##### COUNTY GRAPHS #####
# LOAD DATA
load("rda/covid_ct_counties.rda")
load("rda/theme_DataStache.rda")

library(gghighlight)
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
ggsave(paste("figs/county-cases-", tdy_date, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")

p_hosp_percap_all
ggsave(paste("figs/county-hosp-", tdy_date, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")

p_new_deaths_percap_all
ggsave(paste("figs/county-deaths-", tdy_date, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")

p_hosp_current
ggsave(paste("figs/county-hosp-current-", tdy_date, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")


##### STATE MAPS #####
# LOAD DATA
load("rda/covid_ct_counties_sum.rda")
load("rda/covid_ct_counties_zone.rda")
load("rda/theme_DataStache.rda")

library(usmap)
covid_ct_map_x <- merge(covid_ct_counties_sum, covid_ct_counties_zone, by = c("county", "sum_cases_percap", "percent_pos"))
covid_ct_map_x$county <- fips("CT", covid_ct_map_x$county)

##### NEW CASE HEAT MAP #####
covid_ct_map <- covid_ct_map_x %>%
  rename(fips = county) %>%
  select(fips, sum_cases_percap)

P1 <- plot_usmap("counties",
                 data = covid_ct_map,
                 color = "dark red", 
                 labels = TRUE,
                 include = "CT",
                 values = "sum_cases_percap") +
  scale_fill_gradient(low = "white", high = "dark red", name = "New Cases\nPer 100k", expand = c(0,0)) +
  ggtitle("Heat Map of New Cases in Connecticut Over\nThe Last Seven Days") +
  theme_DataStache() +
  theme(plot.title = element_text(margin = margin(t=5, unit="pt")))+
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        legend.position = "right")

P1$layers[[2]]$aes_params$size <- 2
P1

p_width <- 6
p_height <- (9/16) * p_width 

ggsave(paste("figs/heat-map-cases-", tdy_date, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")


##### NEW HOSPITALIZATION HEAT MAP #####
covid_ct_map <- covid_ct_map_x %>%
  rename(fips = county) %>%
  select(fips, sum_hosp_percap)

P2 <- plot_usmap("counties",
                 data = covid_ct_map,
                 color = "black", 
                 labels = TRUE,
                 include = "CT",
                 values = "sum_hosp_percap") +
  scale_fill_gradient2(low = "dark blue", mid = "white", high = "dark red", name = "New Hospitalization\nPer 100k", expand = c(0,0)) +
  ggtitle("Heat Map of New Hospitalization in Connecticut Over\nThe Last Seven Days") +
  theme_DataStache() +
  theme(plot.title = element_text(margin = margin(t=5, unit="pt")))+
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        legend.position = "right")

P2$layers[[2]]$aes_params$size <- 2
P2

p_width <- 6
p_height <- (9/16) * p_width  

ggsave(paste("figs/heat-map-hosp-", tdy_date, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")
