library(broom)
library(tidyverse)
library(caret)

load("rda/theme_DataStache.rda")
load("rda/covid_ct.rda")
load("rda/ct_weather.rda")

head(covid_ct)
head(weather_usa)

# BUILD DATA SET
dat <- covid_ct %>%
  filter(date >= ymd(20200401)) %>%
  select(date, new_cases) %>%
  left_join(ct_weather) %>%
  select(1:3) %>%
  filter(!is.na(tavg)) %>%
  mutate(cases = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         temp = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup() %>%
  select(date, cases, temp)

dat %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point()


# BUILD TRAIN AND TEST SET
y <- dat$cases
set.seed(1, sample.kind = 'Rounding')
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# LOESS MODEL
temp_range <- diff(range(train_set$temp))
span <- 7/temp_range

Ss <- seq(3:79)
acc <- map_df(Ss, function(s){
  span <- s / temp_range
  fit <- loess(cases ~ temp, degree = 1, span = span, data = train_set, control=loess.control(surface="direct"))
  y_hat <- predict(fit, test_set)
  error <- mean((y_hat - test_set$cases)^2)
  tibble(error = error, S = s)
})

acc %>%
  ggplot(aes(S, error)) +
  geom_line() +
  geom_point()

best_s <- Ss[which.min(acc$error)]
span <- best_s / temp_range

fit <- loess(cases ~ temp, degree = 1, span = span, data = train_set, control=loess.control(surface="direct"))
fit

train_set %>% 
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(size = 1, alpha = .5, color = "grey") +
  geom_line(aes(temp, smooth), color = "red")

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$cases)^2)

predict(fit, 62)










# MAX DATE?
max(dat$date)

##### SINCE APRIL PANDEMIC #####
P1 <- dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = tavg_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()
P1

dat %>%
  ggplot(aes(x = tavg_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  facet_wrap(. ~ month(date))

model <- dat %>%
  filter(date >= ymd(20200401)) %>%
  lm(new_cases_07da ~ tavg_07da, data = .)
model

coefs <- tidy(model, conf.int = TRUE)
coefs


##### PREDICTED CASES PER DAY #####
new_temps <- data.frame(tavg_07da = seq(0,60,5))
new_temps <- predict(model, newdata = new_temps) %>%
  cbind(new_temps) %>%
  rename(new_cases_07da_mod = ".")
new_temps <- new_temps[,c(2,1)]
new_temps


##### PLOT PREDICTION VS ACTUAL #####
dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = tavg_07da, y = new_cases_07da)) +
  geom_hline(yintercept = 0, col = "grey60", size = .7) +
  geom_point(size = .5, alpha = .3, col = "dark blue") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0,100,5)) +
  scale_y_continuous(breaks = seq(0,2000,200), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,max(dat$new_cases_07da, na.rm = TRUE) * 1.05)) +
  ggtitle("Does Average Temperature Affect Covid-19 Infection Rates") +
  xlab("7 Day Average Temperature (°F)") +
  ylab("7 Day Average Daily New Cases") +
  theme_DataStache() +
  theme(axis.title.x.bottom = element_text(size = rel(.7), margin = margin(5, 1, 1, 1)),
        axis.title.y.left = element_text(angle = 90, size = rel(.6), margin = margin(1, 7, 1, 1)))

p_width <- 9
p_height <- (9/16) * p_width
ggsave("figs/ct-weather-infection-regression.png",
       width = p_width,
       height = p_height,
       dpi = "retina")
