# LOAD DATA
load("rda/covid_ct.rda")
load("rda/theme_DataStache.rda")
load("rda/ind_xlim_3m.rda")


##### PLOT CHARTS
# NEW CASES
p_new_case <- covid_ct %>%
  ggplot(aes(date, new_cases)) +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_07da), size = .25, col="blue") +
  ggtitle("Connecticut New Cases Total",
          subtitle = "New Cases Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ind_xlim_3m), ylim = c(0,750)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.9)),
        plot.caption = element_text(size = rel(.5)))

# NEW TESTS        
p_new_test <- covid_ct %>%
  ggplot(aes(date, new_tests)) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_07da), size = .25, col="dark green") +
  ggtitle("Connecticut New Tests Total",
          subtitle = "Tests Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ind_xlim_3m)) +
  geom_hline(yintercept = 0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.9)),
        plot.caption = element_text(size = rel(.5)))

# NEW DEATHS
p_new_deaths <- covid_ct %>%
  ggplot(aes(date, new_deaths)) +
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_deaths_07da), size = .25, col="dark red") +
  ggtitle("Connecticut New Deaths Total",
          subtitle = "Deaths Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ind_xlim_3m), ylim = c(0,20)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.9)),
        plot.caption = element_text(size = rel(.5)))

# HOSPITALIZATION
p_hosp <- covid_ct %>%
  ggplot(aes(date, new_hosp)) +
  geom_bar(stat = "identity", fill="yellow4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_hosp_07da), size = .25, col="yellow4") +
  ggtitle(paste("Connecticut Hospitalization"),
          subtitle = "Currently Hospitalized Per Day With Rolling 7 Day Average") +
  labs(caption = "Created by Andrew F. Griffin \n Covid Data from data.ct.gov") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = c(ind_xlim_3m), ylim = c(-20,50)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.9)),
        plot.caption = element_text(size = rel(.5)))

# GRID ARRANGE PLOTS
grid.arrange(p_new_case, p_new_test, p_new_deaths, p_hosp, nrow = 2)

p_width <- 6
p_height <- (9/16) * p_width 

P <- arrangeGrob(p_new_case, p_new_test, p_new_deaths, p_hosp, nrow = 2)
ggsave("figs/State Daily Sum.png",
       P,
       width = p_width,
       height = p_height,
       dpi = "retina")

