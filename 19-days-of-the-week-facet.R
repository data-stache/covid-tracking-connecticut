# LOAD DATA
load("rda/covid_ct.rda")
load("rda/theme_DataStache.rda")
head(covid_ct)

Day <- "Sunday"
covid_ct %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))) %>% 
  group_by(week) %>%
  mutate(weeks = max(date)) %>%
  filter(date >= ymd(20200801)) %>%
  ggplot(aes(x = as.factor(date), y = new_cases), group = day) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_line(group=1, color = "dark blue", size = .3) +
  geom_point(color = "dark blue", size = .3) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_y_continuous(breaks = seq(0, 5000, 200)) +
  ggtitle("New Case Reporting by Day of the Week") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from data.ct.gov") +
  theme_DataStache() +
  theme(axis.text = element_text(size = rel(.3)),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(. ~ day, scales = "free_x") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(1), rel(1), rel(1), rel(1))))

p_width <- 6
p_height <- (9/16) * p_width 

ggsave("figs/5 Day of Week Compare.png",
       width = p_width,
       height = p_height,
       dpi = "retina")
