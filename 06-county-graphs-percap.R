# LOAD DATA
load("rda/covid_ct_counties.rda")
load("rda/theme_DataStache.rda")

## Pick County
cnty <- "New London"


##### PLOT CHARTS
# NEW CASES
p_new_case <- covid_ct_counties %>%
  filter(county == cnty) %>%
  ggplot(aes(date, new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_percap_07da), size = .25, col="blue") +
  ggtitle(paste(cnty, sep = " ", "New Cases"),
          subtitle = "New Cases Per 100k With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.5)),
        axis.text = element_text(size = rel(.9)),
        plot.caption = element_text(size = rel(.7)))

# NEW TESTS        
p_new_test <- covid_ct_counties %>%
  filter(county == cnty) %>%
  ggplot(aes(date, new_tests_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_percap_07da), size = .25, col="dark green") +
  ggtitle(paste(cnty, sep = " ", "New Tests"),
          subtitle = "New Tests Per 100k With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.5)),
        axis.text = element_text(size = rel(.9)),
        plot.caption = element_text(size = rel(.7)))

# NEW DEATHS
p_new_deaths <- covid_ct_counties %>%
  filter(county == cnty) %>%
  ggplot(aes(date, new_deaths_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_deaths_percap_07da), size = .25, col="dark red") +
  ggtitle(paste(cnty, sep = " ", "New Deaths"),
          subtitle = "New Deaths Per 100k With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.5)),
        axis.text = element_text(size = rel(.9)),
        plot.caption = element_text(size = rel(.7)))

# HOSPITALIZATION
p_hosp <- covid_ct_counties %>%
  filter(county == cnty) %>%
  ggplot(aes(date, new_hosp_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="yellow4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_hosp_percap_07da), size = .25, col="yellow4") +
  ggtitle(paste(cnty, sep = " ", "New Hospitalization"),
          subtitle = "Newly Hospitalized Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.9)),
        plot.caption = element_text(size = rel(.5)))

# GRID ARRANGE PLOTS
grid.arrange(p_new_case, p_new_test, p_new_deaths, p_hosp, nrow = 2)

p_width <- 6
p_height <- (9/16) * p_width 

P <- arrangeGrob(p_new_case, p_new_test, p_new_deaths, p_hosp, nrow = 2)
ggsave("figs/County Daily Per Capita.png",
       P,
       width = p_width,
       height = p_height,
       dpi = 600)
