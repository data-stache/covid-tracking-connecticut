load("rda/covid_ct.rda")
load("rda/weather.rda")

dat <- covid_ct %>%
  inner_join(weather)

##### WHOLE PANDEMIC #####
P1 <- dat %>%
  ggplot(aes(x = avg_T_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()
P1

dat %>%
  ggplot(aes(x = avg_T_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  facet_wrap(. ~ month(date))

model <- lm(new_cases_07da ~ avg_T_07da, data = dat)
model

coefs <- tidy(model, conf.int = TRUE)
coefs

new_temps <- data.frame(avg_T_07da = seq(20,60,5))

predict(model, newdata = new_temps)


##### SINCE JULY #####
P2 <- dat %>%
  filter(date >= ymd(20200701)) %>%
  ggplot(aes(x = avg_T_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()
P2

dat %>%
  filter(date >= ymd(20200701)) %>%
  summarize(cor = cor(new_cases_07da, avg_T_07da))

model_jul <- dat %>%
  filter(date >= ymd(20200701)) %>%
  lm(new_cases_07da ~ avg_T_07da, data = .)
model_jul

coefs_jul <- tidy(model_jul, conf.int = TRUE)
coefs_jul

new_temps <- data.frame(avg_T_07da = seq(20,60,5))

predict(model_jul, newdata = new_temps)
