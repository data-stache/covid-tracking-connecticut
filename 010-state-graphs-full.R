# LOAD DATA
load("rda/covid_ct.rda")
load("rda/theme_DataStache.rda")

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

# GRID ARRANGE PLOTS
grid.arrange(p_new_case, p_new_test, p_hosp, p_new_deaths, p_pos, p_new_hosp, nrow = 2)

tdy_date <- covid_ct$date[1]

p_width <- 6
p_height <- (9/16) * p_width 

P <- arrangeGrob(p_new_case, p_new_test, p_hosp, p_new_deaths, p_pos, p_new_hosp, nrow = 2)
ggsave(paste("figs/state-daily-metrics-", tdy_date, ".png", sep = ''),
       P,
       width = p_width,
       height = p_height,
       dpi = "retina")

