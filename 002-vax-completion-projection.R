# Load Libraries ---------------------------------------------------------------
{
  library(lubridate)
  library(tidyverse)
  library(tidylog)
  library(ggplot2)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
}
options(scipen = 999)

RUN_DATE <- Sys.Date()
CT_POP <- sum(930000, 890000, 190000, 170000, 860000, 270000, 150000, 120000)
CT_THRESHOLD <- CT_POP * .7
CT_THRESH_25 <- CT_POP * .25
CT_THRESH_50 <- CT_POP * .50


# Load Data --------------------------------------------------------------------
load("rda/covid_ct.rda")
load("rda/vaccinations.rda")

COUNT <- count(vaccinations) %>%
  .$n

# Exponential ------------------------------------------------------------------
# Pull LM Data
dat <- vaccinations %>%
  select(date, people_fully_vaccinated) %>%
  arrange(date) %>%
  mutate(day_count = seq(1, COUNT, 1),
         week = week(date),
         max_wk = max(week),
         week = abs(week - max_wk),
         week = .5 ^ (week / 2)) %>%
  select(-max_wk)

# Prediction Data Frame
day_count <- seq(1, 365, 1)
day_count <- as.data.frame(day_count)

# Fit and predict
fit_exp <- lm(sqrt(people_fully_vaccinated) ~ day_count, data = dat)
pred_exp <- predict(fit_exp, day_count)
pred_exp <- pred_exp^2

# Count of Days vector
days <- names(pred_exp)

# Day vaccinated pop exceeds threshold
value <- min(pred_exp[pred_exp >= CT_THRESHOLD])
days_subset <- pred_exp == value

# Which day specifically
count <- days[days_subset]
count <- as.integer(count)

# LM Start date
start <- min(dat$date)

# Modeled Date
exp_pred_date <- start + count



# Linear -----------------------------------------------------------------------
# Fit and predict
fit_lm <- lm(people_fully_vaccinated ~ day_count, weights = week, data = dat)
pred_lm <- predict(fit_lm, day_count)

# Count of Days vector
days <- names(pred_lm)

# Day vaccinated pop exceeds threshold
value <- min(pred_lm[pred_lm >= CT_THRESHOLD])
days_subset <- pred_lm == value

# Which day specifically
count <- days[days_subset]
count <- as.integer(count)

# LM Start date
start <- min(dat$date)

# Modeled Date
lm_pred_date <- start + count



# The Window -------------------------------------------------------------------
window <- c(exp_pred_date, lm_pred_date)

window


# Graph of Fits ----------------------------------------------------------------
pred_exp <- as.integer(pred_exp)
pred_lm <- as.integer(pred_lm)

conf_int_hi <- mean(dat$people_fully_vaccinated) + (qnorm(0.975) * sqrt((mean(dat$people_fully_vaccinated)^2 / length(dat$people_fully_vaccinated))))
conf_int_lo <- mean(dat$people_fully_vaccinated) - (qnorm(0.975) * sqrt((mean(dat$people_fully_vaccinated)^2 / length(dat$people_fully_vaccinated))))

dat_visual <- data.frame(day_count,
                         pred_exp,
                         pred_lm)

G <- dat_visual %>%
  left_join(dat) %>%
  select(day_count, date, people_fully_vaccinated, pred_lm, pred_exp) %>%
  mutate(date = seq(ymd(20210112), ymd(20210112) + (max(day_count)-1), 'day'),
         conf_hi = pred_exp + conf_int_hi,
         conf_lo = pred_exp - conf_int_lo) %>%
  ggplot(aes(x = date)) +
  ggtitle('Connecticut Potential Dates to Reach 70% Immunization') +
  geom_ribbon(aes(ymax = conf_hi, ymin = conf_lo), fill = 'blue', alpha = .2) +
  geom_hline(yintercept = CT_THRESHOLD, size = .5, color = 'red') +
  geom_hline(yintercept = CT_THRESH_25, size = .5, color = 'grey') +
  geom_hline(yintercept = CT_THRESH_50, size = .5, color = 'grey') +
  geom_point(aes(y = people_fully_vaccinated), alpha = .5, size = .3) +
  geom_line(aes(y = pred_exp), color = 'red', size = .4) +
  geom_line(aes(y = conf_hi), color = 'darkblue', size = .1) +
  geom_line(aes(y = conf_lo), color = 'darkblue', size = .1) +
  geom_label(aes(x = window[1], y = CT_THRESHOLD, label = window[1]), size = 2) +
  coord_cartesian(ylim = c(0, CT_THRESHOLD + 1000), xlim = c(min(dat$date), exp_pred_date+20)) +
  theme_DataStache()

G

p_width <- 12
p_height <- (9/16) * p_width  

ggsave(paste("figs/", STATE, '-', tdy_date, ".png", sep = ""),
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")


