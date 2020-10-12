# LOAD DATA
load("rda/covid_ct_counties_sum.rda")
load("rda/theme_DataStache.rda")

covid_ct_map <-covid_ct_counties_sum
covid_ct_map$county <- fips("CT", covid_ct_map$county)

covid_ct_map <- covid_ct_map %>%
  rename(fips = county) %>%
  select(fips, sum_cases_percap)


P <- plot_usmap("counties",
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

P$layers[[2]]$aes_params$size <- 2
P

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/CT Heat Map.png",
       width = p_width,
       height = p_height,
       dpi = "retina")
