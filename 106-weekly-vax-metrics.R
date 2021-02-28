library(tidyverse)
library(lubridate)
library(gridExtra)
options(scipen = 999)

load('rda/vaccinations.rda')
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")

tdy_date <- vaccinations$date[1]

names(vaccinations)

covid_ct_weekly <- vaccinations %>%
  group_by(week) %>%
  summarize(new_vaccinations = sum(new_vaccinations),
            new_people_vaccinated = sum(new_people_vaccinated),
            new_fully_vaccinated = sum(new_fully_vaccinated)) %>%
  arrange(desc(week)) %>%
  drop_na()

min_date <- min(covid_ct_weekly$week)
max_date <- max(covid_ct_weekly$week)


## CASES
change <- round(((covid_ct_weekly$new_vaccinations[1] - covid_ct_weekly$new_vaccinations[2]) / covid_ct_weekly$new_vaccinations[2]) * 100, 1)
p_NEW_VACCINATIONS <- covid_ct_weekly %>%
  ggplot(aes(x = week, y = new_vaccinations)) +
  geom_col(fill = 'red4') +
  ggtitle(paste('CT New Vaccinations\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
          subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
  scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                             to = max_date, 
                                             by = "2 weeks"),
               minor_breaks = function(x) seq.Date(from = min_date, 
                                                   to = max_date, 
                                                   by = "week"),
               date_labels = '%b-%d') +
  theme_DataStache() +
  coord_cartesian(expand = FALSE) +
  geom_hline(yintercept = 0, size = .25 , col = "grey40") +
  theme(plot.title = element_text(size = rel(.5), hjust = .5),
        plot.subtitle = element_text(size = rel(.5), face = 'bold',
                                     hjust = .5,
                                     vjust = 1.5, colour = 'red4'),
        axis.text.y = element_text(size = rel(1),
                                   face = "bold"),
        axis.text.x = element_text(size = rel(.8),
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(.1, .2, .1, .2), "cm"))

p_NEW_VACCINATIONS


## TESTS
change <- round(((covid_ct_weekly$new_people_vaccinated[1] - covid_ct_weekly$new_people_vaccinated[2]) / covid_ct_weekly$new_people_vaccinated[2]) * 100, 1)
p_NEW_VAX <- covid_ct_weekly %>%
  ggplot(aes(x = week, y = new_people_vaccinated)) +
  geom_col(fill = 'magenta4') +
  ggtitle(paste('CT New People Vaccinated\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
          subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
  scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                             to = max_date, 
                                             by = "2 weeks"),
               minor_breaks = function(x) seq.Date(from = min_date, 
                                                   to = max_date, 
                                                   by = "week"),
               date_labels = '%b-%d') +
  theme_DataStache() +
  coord_cartesian(expand = FALSE) +
  geom_hline(yintercept = 0, size = .25 , col = "grey40") +
  theme(plot.title = element_text(size = rel(.5), hjust = .5),
        plot.subtitle = element_text(size = rel(.5), face = 'bold',
                                     hjust = .5,
                                     vjust = 1.5, colour = 'magenta4'),
        axis.text.y = element_text(size = rel(1),
                                   face = "bold"),
        axis.text.x = element_text(size = rel(.8),
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(.1, .2, .1, .2), "cm"))

p_NEW_VAX


## HOSP
change <- round(((covid_ct_weekly$new_fully_vaccinated[1] - covid_ct_weekly$new_fully_vaccinated[2]) / covid_ct_weekly$new_fully_vaccinated[2]) * 100, 1)
p_FULLY_VAX <- covid_ct_weekly %>%
  ggplot(aes(x = week, y = new_fully_vaccinated)) +
  geom_col(fill = 'dark blue') +
  ggtitle(paste('CT New People Fully Vaccinated\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
          subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
  scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                             to = max_date, 
                                             by = "2 weeks"),
               minor_breaks = function(x) seq.Date(from = min_date, 
                                                   to = max_date, 
                                                   by = "week"),
               date_labels = '%b-%d') +
  theme_DataStache() +
  coord_cartesian(expand = FALSE) +
  geom_hline(yintercept = 0, size = .25 , col = "grey40") +
  theme(plot.title = element_text(size = rel(.5), hjust = .5),
        plot.subtitle = element_text(size = rel(.5), face = 'bold',
                                     hjust = .5,
                                     vjust = 1.5, colour = 'dark blue'),
        axis.text.y = element_text(size = rel(1),
                                   face = "bold"),
        axis.text.x = element_text(size = rel(.8),
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(.1, .2, .1, .2), "cm"))

p_FULLY_VAX



grid.arrange(p_NEW_VACCINATIONS, p_NEW_VAX, p_FULLY_VAX, nrow = 1)
P <- arrangeGrob(p_NEW_VACCINATIONS, p_NEW_VAX, p_FULLY_VAX, nrow = 1)

p_width <- 6
p_height <- (9/16) * p_width 

ggsave(paste("figs/CT-weekly-vax-metrics-", tdy_date,".png", sep = ''),
       P,
       width = p_width,
       height = p_height,
       dpi = 600)


