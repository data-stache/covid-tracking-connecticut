# LOAD DATA
load("rda/covid_ct_towns.rda")
load("rda/theme_DataStache.rda")
load("rda/ind_tylm_date.rda")
load("rda/covid_ct_towns_sum.rda")

## Pick Town
twn <- "New Haven"


##### PLOT CHARTS
# NEW CASES
p_new_case_town <- covid_ct_towns %>%
  filter(town == twn) %>%
  ggplot(aes(date, new_cases)) +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_07da), size = .5, col="blue") +
  ggtitle(paste(twn, sep = " ", "New Cases")) +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(face="bold", color="black", size=14),
        plot.subtitle = element_text(color="black", size=10),
        plot.caption = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle=90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_line(color = "grey 80", size = (.3)),
        legend.position = "none")

# NEW TESTS        
p_new_test_town <- covid_ct_towns %>%
  filter(town == twn) %>%
  ggplot(aes(date, new_tests)) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_07da), size = .5, col="dark green") +
  ggtitle(paste(twn, sep = " ", "New Tests")) +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(face="bold", color="black", size=14),
        plot.subtitle = element_text(color="black", size=10),
        plot.caption = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle=90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_line(color = "grey 80", size = (.3)),
        legend.position = "none")

# NEW DEATHS
p_new_deaths_town <- covid_ct_towns %>%
  filter(town == twn) %>%
  ggplot(aes(date, new_deaths)) +
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_deaths_07da), size = .5, col="dark red") +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  ggtitle(paste(twn, sep = " ", "New Deaths")) +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0, NA) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(face="bold", color="black", size=14),
        plot.subtitle = element_text(color="black", size=10),
        plot.caption = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle=90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_line(color = "grey 80", size = (.3)),
        legend.position = "none")

# GRID ARRANGE PLOTS
grid.arrange(p_new_case_town, p_new_test_town, p_new_deaths_town, nrow = 1)


# OVERVIEW
covid_ct_towns %>%
  select(date, town, new_cases, new_tests, percent_pos, new_deaths) %>%
  filter(town == twn) %>%
  filter(date %in% ind_tylm_date)

covid_ct_towns_sum %>%
  filter(town == twn)
