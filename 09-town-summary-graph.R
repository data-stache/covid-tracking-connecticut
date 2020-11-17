library(tidyverse)
library(gridExtra)

# LOAD DATA
load("rda/covid_ct_towns.rda")
load("rda/theme_DataStache.rda")
load("rda/ind_tylm_date.rda")
load("rda/covid_ct_towns_sum.rda")
load("rda/theme_DataStache.rda")

## Pick Town
twn <- "New Haven"


##### PLOT CHARTS
# NEW CASES
p_new_case_town <- covid_ct_towns %>%
  filter(town == twn) %>%
  ggplot(aes(date, new_cases)) +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_07da), size = .35, col="blue") +
  ggtitle(paste('City of', twn, sep = " ", "New Cases")) +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  geom_hline(yintercept=0, col = "grey40", size = .3) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# NEW TESTS        
p_new_test_town <- covid_ct_towns %>%
  filter(town == twn) %>%
  ggplot(aes(date, new_tests)) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_07da), size = .35, col="dark green") +
  ggtitle(paste('City of', twn, sep = " ", "New Tests")) +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  geom_hline(yintercept=0, col = "grey40", size = .3) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# PERCENT POS        
p_per_pos_town <- covid_ct_towns %>%
  filter(town == twn) %>%
  ggplot(aes(date, percent_pos)) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = percent_pos_07da), size = .35, col="dark green") +
  ggtitle(paste('City of', twn, sep = " ", "Percent Positive")) +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  geom_hline(yintercept=0, col = "grey40", size = .3) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# NEW DEATHS
p_new_deaths_town <- covid_ct_towns %>%
  filter(town == twn) %>%
  ggplot(aes(date, new_deaths)) +
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_deaths_07da), size = .35, col="dark red") +
  geom_hline(yintercept=0, col = "grey40", size = .3) +
  ggtitle(paste('City of', twn, sep = " ", "New Deaths")) +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# GRID ARRANGE PLOTS
grid.arrange(p_new_case_town, p_new_test_town, p_per_pos_town, p_new_deaths_town, nrow = 2)

p_width <- 6
p_height <- (9/16) * p_width 

P <- arrangeGrob(p_new_case_town, p_new_test_town, p_per_pos_town, p_new_deaths_town, nrow = 2)
ggsave(paste("figs/", twn, "-daily-summary-", tdy_date, ".png", sep = ""),
       P,
       width = p_width,
       height = p_height,
       dpi = "retina")

# OVERVIEW
covid_ct_towns %>%
  select(date, town, new_cases, new_tests, percent_pos, new_deaths) %>%
  filter(town == twn) %>%
  filter(date %in% ind_tylm_date) %>%
  kable()

covid_ct_towns %>%
  select(date, town, new_cases_07da, new_tests_07da, percent_pos_07da, new_deaths_07da) %>%
  filter(town == twn) %>%
  filter(date %in% ind_tylm_date) %>%
  kable()

covid_ct_towns_sum %>%
  filter(town == twn) %>%
  kable()
