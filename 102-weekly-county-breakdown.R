library(tidyverse)
library(lubridate)
library(gridExtra)

load('rda/covid_ct_counties.rda')
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")

tdy_date <- covid_ct_counties$date[1]

covid_ct_county_weekly <- covid_ct_counties %>%
  filter(date >= ymd(20200329)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, county) %>%
  summarize(new_cases = sum(new_cases),
            new_tests = sum(new_tests),
            avg_hos = mean(hospitalization),
            new_death = sum(new_deaths))

week_start <- covid_ct_counties %>%
  filter(date >= ymd(20200329)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week) %>%
  arrange(date) %>%
  slice_head(., n = 1) %>%
  arrange(desc(date)) %>%
  select(week, date) %>%
  ungroup()

covid_ct_county_weekly <- covid_ct_county_weekly %>% inner_join(week_start) %>% arrange(desc(date))
min_date <- min(covid_ct_county_weekly$date)
max_date <- max(covid_ct_county_weekly$date)

cnty <- levels(covid_ct_counties$county)

weekly_summary_function <- function(cnty) {
  # FILTER COUNTY
  covid_ct_weekly <- covid_ct_county_weekly %>% filter(county == cnty)
  
  ## CASES
  change <- round(((covid_ct_weekly$new_cases[1] - covid_ct_weekly$new_cases[2]) / covid_ct_weekly$new_cases[2]) * 100, 1)
  p_Cases <- covid_ct_weekly %>%
    ggplot(aes(x = date, y = new_cases)) +
    geom_col(fill = 'red4') +
    ggtitle(paste(cnty, 'Weekly Cases\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
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
  
  ## TESTS
  change <- round(((covid_ct_weekly$new_tests[1] - covid_ct_weekly$new_tests[2]) / covid_ct_weekly$new_tests[2]) * 100, 1)
  p_Tests <- covid_ct_weekly %>%
    ggplot(aes(x = date, y = new_tests)) +
    geom_col(fill = 'magenta4') +
    ggtitle(paste(cnty, 'Weekly Tests\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
            subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
    scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                               to = max_date, 
                                               by = "2 weeks"),
                 minor_breaks = function(x) seq.Date(from = min_date, 
                                                     to = max_date, 
                                                     by = "week"),
                 date_labels = '%b-%d') +
    theme_DataStache() +
    coord_cartesian(xlim = c(ymd(20200329), NA), expand = FALSE) +
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
  
  ## HOSP
  change <- round(((covid_ct_weekly$avg_hos[1] - covid_ct_weekly$avg_hos[2]) / covid_ct_weekly$avg_hos[2]) * 100, 1)
  p_Hosp <- covid_ct_weekly %>%
    ggplot(aes(x = date, y = avg_hos)) +
    geom_col(fill = 'dark blue') +
    ggtitle(paste(cnty, 'Avg Hospitalization\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
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
  
  ## DEATH
  change <- round(((covid_ct_weekly$new_death[1] - covid_ct_weekly$new_death[2]) / covid_ct_weekly$new_death[2]) * 100, 1)
  p_Death <- covid_ct_weekly %>%
    ggplot(aes(x = date, y = new_death)) +
    geom_col(fill = 'green4') +
    ggtitle(paste(cnty, 'Weekly Death\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
            subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
    scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                               to = max_date, 
                                               by = "2 weeks"),
                 minor_breaks = function(x) seq.Date(from = min_date, 
                                                     to = max_date, 
                                                     by = "week"),
                 date_labels = '%b-%d') +
    theme_DataStache() +
    coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
    geom_hline(yintercept = 0, size = .25 , col = "grey40") +
    theme(plot.title = element_text(size = rel(.5), hjust = .5),
          plot.subtitle = element_text(size = rel(.5), face = 'bold',
                                       hjust = .5,
                                       vjust = 1.5, colour = 'green4'),
          axis.text.y = element_text(size = rel(1),
                                   face = "bold"),
          axis.text.x = element_text(size = rel(.8),
                                     angle = 90),
          panel.grid.major.x = element_blank(),
          plot.margin = unit(c(.1, .2, .1, .2), "cm"))

  grid.arrange(p_Cases, p_Tests, p_Hosp, p_Death, nrow = 1)
  P <- arrangeGrob(p_Cases, p_Tests, p_Hosp, p_Death, nrow = 1)

  p_width <- 6
  p_height <- (9/16) * p_width 

  ggsave(paste('figs/', cnty, '-weekly-metrics-', tdy_date, '.png', sep = ''),
         P,
         width = p_width,
         height = p_height,
         dpi = 'retina')
}

lapply(cnty, weekly_summary_function)
