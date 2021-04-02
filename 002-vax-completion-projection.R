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
  mutate(day_count = seq(1, COUNT, 1))

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
# Pull LM Data
dat <- vaccinations %>%
  select(date, people_fully_vaccinated) %>%
  arrange(date) %>%
  mutate(day_count = seq(1, COUNT, 1))

# Prediction Data Frame
day_count <- seq(1, 365, 1)
day_count <- as.data.frame(day_count)

# Fit and predict
fit_lm <- lm(people_fully_vaccinated ~ day_count, data = dat)
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




