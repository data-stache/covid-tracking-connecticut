# Load Libraries ---------------------------------------------------------------
{
  library(lubridate)
  library(tidyverse)
  library(tidylog)
  library(ggplot2)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
}



# Load Data --------------------------------------------------------------------
load("rda/covid_ct.rda")
load("rda/vaccinations.rda")



# Variables --------------------------------------------------------------------
RUN_DATE <- Sys.Date()

head(vaccinations)

# Make Grpahs ------------------------------------------------------------------
# New Vaccinations
p_new_vac <- vaccinations %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(aes(y = new_vaccinations), stat = "identity", fill="red", alpha = .3, size = .1) +
  geom_line(aes(y = new_vaccinations_07da), size = .2, col="red") +
  ggtitle("New Doses Used",
          subtitle = 'State of Connecticut') +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_x_date(date_labels = "%b %d", date_breaks= "2 week") +
  scale_y_continuous(breaks = seq(0, 70000, 5000), expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 55000)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.45)),
        axis.text.x = element_text(angle = 90),
        plot.caption = element_text(size = rel(.5)))

p_new_peeps <- vaccinations %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(aes(y = new_people_vaccinated), stat = "identity", fill="blue", alpha = .3, size = .1) +
  geom_line(aes(y = new_people_vaccinated_07da), size = .2, col="blue") +
  ggtitle("New First Dose",
          subtitle = 'State of Connecticut') +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_x_date(date_labels = "%b %d", date_breaks= "2 week") +
  scale_y_continuous(breaks = seq(0, 70000, 5000), expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 55000)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.45)),
        axis.text.x = element_text(angle = 90),
        plot.caption = element_text(size = rel(.5)))

# New Fully Vaccinated     
p_new_full_vac <- vaccinations %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(aes(y = new_fully_vaccinated), stat = "identity", fill="dark green", alpha = .3, size = .1) +
  geom_line(aes(y = new_fully_vaccinated_07da), size = .2, col="dark green") +
  ggtitle("New Fully Vaccinated",
          subtitle = 'State of Connecticut') +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_x_date(date_labels = "%b %d", date_breaks= "2 week") +
  scale_y_continuous(breaks = seq(0, 70000, 5000), expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 55000)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.45)),
        axis.text.x = element_text(angle = 90),
        plot.caption = element_text(size = rel(.5)))

# Share Vaccinated
p_share_vaccinated <- vaccinations %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_line(aes(y = share_pop_vaccinated), size = .2, col="dark red") +
  geom_line(aes(y = share_pop_fully_vaccinated), size = .2, col="dark orange") +
  ggtitle("Share Vaccinated",
          subtitle = 'State of Connecticut') +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_x_date(date_labels = "%b %d", date_breaks= "2 week") +
  scale_y_continuous(expand = c(0,0), breaks = (seq(0, 1, .05))) +
  coord_cartesian(ylim = c(NA, .4)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.45)),
        axis.text.x = element_text(angle = 90),
        plot.caption = element_text(size = rel(.5)))


library(gridExtra)
# GRID ARRANGE PLOTS
grid.arrange(p_new_vac, p_new_peeps, p_new_full_vac, p_share_vaccinated, nrow = 1)

p_width <- 6
p_height <- (9/16) * p_width 

P <- arrangeGrob(p_new_vac, p_new_peeps, p_new_full_vac, p_share_vaccinated, nrow = 1)
ggsave(paste("figs/state-daily-vac-metrics-", RUN_DATE, ".png", sep = ""),
       P,
       width = p_width,
       height = p_height,
       dpi = "retina")



