library(broom)
library(tidyverse)
load("rda/covid_ct.rda")
load("rda/weather.rda")

dat <- covid_ct %>%
  inner_join(ct_weather)

##### SINCE APRIL PANDEMIC #####
P1 <- dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = avg_t_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()
P1

dat %>%
  ggplot(aes(x = avg_t_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  facet_wrap(. ~ month(date))

model <- dat %>%
  filter(date >= ymd(20200401)) %>%
  lm(new_cases_07da ~ avg_t_07da, data = .)
model

coefs <- tidy(model, conf.int = TRUE)
coefs

new_temps <- data.frame(avg_t_07da = seq(20,60,5))

predict(model, newdata = new_temps)



