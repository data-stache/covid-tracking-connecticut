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

ggsave("figs/CT Heat Map Cases.png",
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

ggsave("figs/CT Heat Map Hosp.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

##### CASES ZONE #####
covid_ct_map <- covid_ct_map_x %>%
  rename(fips = county) %>%
  mutate(cases_zone = factor(cases_zone, levels = c("Green", "Yellow", "Red"))) %>%
  select(fips, cases_zone)

P3 <- plot_usmap("counties",
                 data = covid_ct_map,
                 color = "black", 
                 labels = TRUE,
                 include = "CT",
                 values = "cases_zone") +
  scale_fill_manual(values = c("Green4", "Yellow3", "Red"), name = "Zone", expand = c(0,0)) +
  ggtitle("Zone of County Based on New Cases Over 7 Days") +
  theme_DataStache() +
  theme(plot.title = element_text(margin = margin(t=5, unit="pt")))+
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        legend.position = "right")

P3$layers[[2]]$aes_params$size <- 2
P3

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/CT Heat Positive Cases.png",
       width = p_width,
       height = p_height,
       dpi = "retina")


##### POSITIVE ZONE #####
covid_ct_map <- covid_ct_map_x %>%
  rename(fips = county) %>%
  mutate(percent_zone = factor(percent_zone, levels = c("Green", "Yellow", "Red"))) %>%
  select(fips, percent_zone)

P4 <- plot_usmap("counties",
                 data = covid_ct_map,
                 color = "black", 
                 labels = TRUE,
                 include = "CT",
                 values = "percent_zone") +
  scale_fill_manual(values = c("Green4", "Yellow3", "Red"), name = "Zone", expand = c(0,0)) +
  ggtitle("Zone of County Based on Percent Positive Average Over 7 Dys") +
  theme_DataStache() +
  theme(plot.title = element_text(margin = margin(t=5, unit="pt")))+
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        legend.position = "right")

P4$layers[[2]]$aes_params$size <- 2
P4

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/CT Heat Positive Zone.png",
       width = p_width,
       height = p_height,
       dpi = "retina")
