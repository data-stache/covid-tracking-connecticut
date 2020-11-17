##### LOAD THE DATA ##### 
# CT TOWNSHIP CODE TO COUNTY
town_county_code <- read.csv("data/town_id_county.csv")
save(town_county_code, file = "rda/town_county_code.rda")

# CT COUNTY POPULATIONS
populations <- data.frame(county = c("Fairfield", "Hartford", "Litchfield", "Middlesex", "New Haven", "New London", "Tolland", "Windham"),
                          population = c(930000, 890000, 190000, 170000, 860000, 270000, 150000, 120000))
save(populations, file = "rda/populations.rda")

##### CENTRALIZE DATA ##### 
# ADD COUNTY TO TOWNSHPS
covid_ct <- read.csv("data/covid-daily-town-report-connecticut.csv")
county_hospitalization <- read.csv("data/covid-daily-county-report-connecticut.csv")

covid_ct <- covid_ct %>%
  mutate(county = case_when(town_no %in% town_county_code$FAIRFIELD ~ "Fairfield",
                            town_no %in% town_county_code$HARTFORD ~ "Hartford",
                            town_no %in% town_county_code$LITCHFIELD ~ "Litchfield",
                            town_no %in% town_county_code$MIDDLESEX ~ "Middlesex",
                            town_no %in% town_county_code$NEW.HAVEN ~ "New Haven",
                            town_no %in% town_county_code$NEW.LONDON ~ "New London",
                            town_no %in% town_county_code$TOLLAND ~ "Tolland",
                            town_no %in% town_county_code$WINDHAM ~ "Windham"))

# REFORMAT DATE
covid_ct <- covid_ct %>%
  rename("date" = "lastupdatedate") 
covid_ct$date <- ymd(covid_ct$date)

county_hospitalization <- county_hospitalization %>%
  separate(dateupdated, "date", " ", extra = "drop")
county_hospitalization$date <- ymd(county_hospitalization$date) 

# ADD NEW CASES / TESTS / DEATHS
covid_ct_towns <- covid_ct %>%
  group_by(town_no) %>%
  mutate(new_cases = towntotalcases - lag(towntotalcases, n = 1, order_by = date), # NEW CASES
         new_tests = numberoftests - lag(numberoftests, n = 1, order_by = date), # NEW TESTS
         new_deaths = towntotaldeaths - lag(towntotaldeaths, n = 1, order_by = date)) %>% # NEW DEATHS
  ungroup()

# ARRANGE BY DATE (MOST RECENT -> OLDEST)
covid_ct_towns <- covid_ct_towns %>%
  arrange(desc(date))

# ADD DAY OF THE WEEK
covid_ct_towns <- covid_ct_towns %>%
  mutate(day = weekdays(date))



##### BUILD DATA SET FOR COUNTIES ##### 
# SELECT RELEVANT COLUMNS
covid_ct_counties <- covid_ct_towns %>% 
  select(date, county, new_cases, new_tests, new_deaths)

# MAKE COUNTIES A FACTOR
covid_ct_counties <- covid_ct_counties %>%
  mutate(county = as.factor(county)) 

# COUNTY TOTAL NEW CASES / TESTS / DEATHS 
covid_ct_counties <- covid_ct_counties %>%
  group_by(county, date) %>%
  summarise(new_cases = sum(new_cases), 
            new_tests = sum(new_tests),
            new_deaths = sum(new_deaths)) %>%
  mutate(day = weekdays(date)) %>%
  ungroup()

### MERGE HOSPITALIZATION
# GRAB WHAT I NEED FOR HOSPITALIZATION
county_hospitalization <- county_hospitalization %>%
  select(date, county, hospitalization)

covid_ct_counties <- merge(covid_ct_counties, county_hospitalization, by = c("date","county")) %>%
  # ADD NEW HOSPITALIZATION
  group_by(county) %>%
  mutate(new_hosp = hospitalization - lag(hospitalization, n = 1, order_by = date))
         
# COUNTY ADD PERCENT SHARE POSITIVE
covid_ct_counties <- covid_ct_counties %>%
  mutate(percent_pos = round(new_cases / new_tests, 4))

# COUNTY ADD POPULATION DATA
covid_ct_counties <- merge(covid_ct_counties, populations, by = "county")

# COUNTY ADD PER CAPITA DATA
covid_ct_counties <- covid_ct_counties %>%
  mutate(new_cases_percap = round(new_cases / population * 100000, 1),
         new_tests_percap = round(new_tests / population * 100000, 1),
         new_deaths_percap = round(new_deaths / population * 100000, 1),
         hosp_percap = round(hospitalization / population * 100000, 1),
         new_hosp_percap = round(new_hosp / population * 100000, 1))

# COUNTY ARRANGE BY DATE (MOST RECENT -> OLDEST)
covid_ct_counties <- covid_ct_counties %>%
  arrange(desc(date))

# COUNTY ADD 7 DAY ROLLING AVERAGES
covid_ct_counties <- covid_ct_counties %>%
  group_by(county) %>%
  mutate(new_cases_07da = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_tests_07da = rollapply(new_tests, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_deaths_07da = rollapply(new_deaths, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_hosp_07da = rollapply(new_hosp, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"),
         new_cases_percap_07da = rollapply(new_cases_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_tests_percap_07da = rollapply(new_tests_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_deaths_percap_07da = rollapply(new_deaths_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_hosp_percap_07da = rollapply(new_hosp_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"),) %>%
  ungroup()

# COUNTY COLUMN ORDER
col_order <- c("date", "day", "county", "population", "new_cases", "new_tests", "new_deaths", "new_hosp", "hospitalization", "new_cases_percap", "new_tests_percap",
               "new_deaths_percap", "new_hosp_percap", "new_cases_07da", "new_tests_07da", "new_deaths_07da", "new_hosp_07da", "new_cases_percap_07da", "new_tests_percap_07da", 
               "new_deaths_percap_07da", "new_hosp_percap_07da", "percent_pos")

# COUNTY REORDER COLUMNS
covid_ct_counties <- covid_ct_counties[,col_order]

# COUNTY ARRANGE BY DATE (MOST RECENT -> OLDEST)
covid_ct_counties <- covid_ct_counties %>%
  arrange(desc(date))
save(covid_ct_counties, file = "rda/covid_ct_counties.rda")


##### BUILD DATA SET FOR TOWNS ##### 
# MAKE TOWNS A FACTOR
covid_ct_towns <- covid_ct_towns %>%
  mutate(town = as.factor(town)) 

# SELECT RELEVANT COLUMNS
covid_ct_towns <- covid_ct_towns %>% 
  select(date, day, town, county, new_cases, new_tests, new_deaths)

# ADD PERCENT SHARE POSITIVE
covid_ct_towns <- covid_ct_towns %>%
  mutate(percent_pos = new_cases / new_tests)

# ARRANGE BY DATE (MOST RECENT -> OLDEST)
covid_ct_towns <- covid_ct_towns %>%
  arrange(town, desc(date))

# ADD 7 DAY ROLLING AVERAGES
covid_ct_towns <- covid_ct_towns %>%
  group_by(town) %>%
  mutate(new_cases_07da = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_tests_07da = rollapply(new_tests, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_deaths_07da = rollapply(new_deaths, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         percent_pos_07da = rollapply(percent_pos, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")) %>%
  ungroup()

# COLUMN ORDER
col_order_2 <- c("date", "day", "town", "county", "new_cases", "new_tests", "new_deaths", "new_cases_07da", "new_tests_07da", "new_deaths_07da", "percent_pos", "percent_pos_07da")

# REORDER COLUMNS
covid_ct_towns <- covid_ct_towns[,col_order_2]

# ARRANGE BY DATE (MOST RECENT -> OLDEST)
covid_ct_towns <- covid_ct_towns %>%
  arrange(desc(date))
save(covid_ct_towns, file = "rda/covid_ct_towns.rda")



##### BUILD DATE INDEXS ##### 
ind_tdy <- covid_ct_counties$date[1] # TODAY
save(ind_tdy, file = "rda/ind_tdy.rda")

ind_tylm_date <- c(ind_tdy, ind_tdy - 1, ind_tdy - 7, ind_tdy - 28) # PULL DATES OF T/Y/LW/LM
save(ind_tylm_date, file = "rda/ind_tylm_date.rda")

ind_mnth <- ind_tdy - 28 # 4 WEEKS AGO
save(ind_mnth, file = "rda/ind_mnth.rda")

ind_days_90 <- ind_tdy - 90 # 90 DAYS AGO
save(ind_days_90, file = "rda/ind_days_90.rda")

ind_wk <- ind_tdy - 7 # 7 DAYS AGO
save(ind_wk, file = "rda/ind_wk.rda")

ind_2wk <- ind_tdy - 14 # 7 DAYS AGO
save(ind_2wk, file = "rda/ind_2wk.rda")

ind_yale_rtrn <- ymd(20200824) # DATE YALE UNDERGRADS BEGIN TO RETURN
save(ind_yale_rtrn, file = "rda/ind_yale_rtrn.rda")

# 3 MONTH LIMIT SET
xlim1 <- ind_days_90
xlim2 <- ind_tdy
ind_xlim_3m <- c(xlim1, xlim2)
save(ind_xlim_3m, file = "rda/ind_xlim_3m.rda")

##### SUMMARISE NEW CASES, TESTS, HOSPITALIZATION, AND DEATHS BY COUNTY ##### 
covid_ct_counties_sum <- covid_ct_counties %>%
  # FILTER LAST 7 DAYS
  filter(date > ind_wk) %>%
  # GROUP BY COUNTY
  group_by(county) %>%
  # SUMMARIZE NEW CASES
  summarize(sum_cases_percap = sum(new_cases_percap),
            sum_tests_percap = sum(new_tests_percap),
            sum_deaths_percap = sum(new_deaths_percap),
            sum_hosp_percap = sum(new_hosp_percap),
            percent_pos = mean(percent_pos)) %>%
  ungroup()
save(covid_ct_counties_sum, file = "rda/covid_ct_counties_sum.rda")

# COUNTY ORDER INDEX
ind_new_case_county <- c(order(desc(covid_ct_counties_sum$sum_cases_percap)))
save(ind_new_case_county, file = "rda/ind_new_case_county.rda")

ind_new_test_county <- c(order(desc(covid_ct_counties_sum$sum_tests_percap)))
save(ind_new_test_county, file = "rda/ind_new_test_county.rda")

ind_new_death_county <- c(order(desc(covid_ct_counties_sum$sum_deaths_percap)))
save(ind_new_death_county, file = "rda/ind_new_death_county.rda")

ind_new_hosp_county <- c(order(desc(covid_ct_counties_sum$sum_hosp_percap)))
save(ind_new_hosp_county, file = "rda/ind_new_hosp_county.rda")


##### COUNTY GREEN / YELLOW / RED ZONE ####
# ZONE FUNCTION
# PERCENT POS FUNCTION
fct_positive_zone <- function(x) { 
  if(x < .05) {
    print("Green")
  } else if (x >= .05 & x < .1) {
    print("Yellow")
  } else if (x >= .1) {
    print("Red")
  }}

# CASES FUNCTION
fct_cases_zone <- function(x) { 
  if(x < 10) {
    print("Green")
  } else if (x >= 10 & x < 100) {
    print("Yellow")
  } else if (x >= 100) {
    print("Red")
  }}

covid_ct_counties_zone <- covid_ct_counties %>%
  filter(date > ind_wk) %>%
  group_by(county) %>%
  summarize(sum_cases_percap = sum(new_cases_percap),
            percent_pos = mean(percent_pos, na.rm = TRUE),
            cases_zone = fct_cases_zone(sum_cases_percap),
            percent_zone = fct_positive_zone(percent_pos)) %>%
  ungroup()
save(covid_ct_counties_zone, file = "rda/covid_ct_counties_zone.rda")

##### SUMMARISE NEW CASES, TESTS, AND DEATHS BY TOWN ##### 
covid_ct_towns_sum <- covid_ct_towns %>% 
  # FILTER LAST 7 DAYS
  filter(date > ind_wk) %>% 
  # GROUP BY TOWN
  group_by(town) %>%
  # SUMMARIZE NEW CASES
  summarize(sum_new_cases = sum(new_cases),
            sum_new_tests = sum(new_tests),
            sum_new_deaths = sum(new_deaths)) %>%
  mutate(percent_pos = sum_new_cases / sum_new_tests) %>%
  mutate(rank = rank(-sum_new_cases)) %>%
  ungroup()

# ADD COUNTY
covid_ct_towns_county <- covid_ct_towns %>%
  filter(date == ind_tdy) %>%
  select(town, county)

covid_ct_towns_sum <- merge(covid_ct_towns_sum, covid_ct_towns_county, by = "town")
save(covid_ct_towns_sum, file = "rda/covid_ct_towns_sum.rda")

# TOWN ORDER INDEX
ind_new_case_town <- c(order(desc(covid_ct_towns_sum$sum_new_cases)))
save(ind_new_case_town, file = "rda/ind_new_case_town.rda")

ind_new_test_town <- c(order(desc(covid_ct_towns_sum$sum_new_tests)))
save(ind_new_test_town, file = "rda/ind_new_test_town.rda")

ind_new_death_town <- c(order(desc(covid_ct_towns_sum$sum_new_deaths)))
save(ind_new_death_town, file = "rda/ind_new_death_town.rda")


##### SUMMARIZED DATA ALL CT #####
covid_ct <- covid_ct_counties %>%
  group_by(date) %>%
  summarize(new_cases = sum(new_cases),
            new_tests = sum(new_tests),
            percent_pos = new_cases / new_tests,
            new_deaths = sum(new_deaths),
            current_hosp = sum(hospitalization),
            new_hosp = sum(new_hosp)) %>%
  ungroup() %>%
  mutate(new_cases_07da = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_tests_07da = rollapply(new_tests, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         percent_pos_07da = rollapply(percent_pos, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_deaths_07da = rollapply(new_deaths, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         cur_hosp_07da = rollapply(current_hosp, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_hosp_07da = rollapply(new_hosp, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         day = weekdays(date)) %>%
  arrange(desc(date))
covid_ct <- covid_ct[c(1, 14, 2:13)]
save(covid_ct, file = "rda/covid_ct.rda")

##### SUMMARISE NEW CASES, TESTS, AND DEATHS BY STATE ##### 
covid_ct_sum <- covid_ct %>% 
  # FILTER LAST 7 DAYS
  filter(date > ind_2wk) %>%
  # 1wk 2wk
  mutate(week = ifelse(date > ind_wk, "Last 7 Days", "Last Week")) %>%
  # GROUP BY WEEK
  group_by(week) %>%
  # SUMMARIZE NEW CASES
  summarize(sum_new_cases = sum(new_cases),
            sum_new_tests = sum(new_tests),
            sum_new_deaths = sum(new_deaths),
            m_hosp = mean(current_hosp),
            sum_new_hosp = sum(new_hosp)) %>%
  mutate(percent_pos = sum_new_cases / sum_new_tests) %>%
  ungroup()
save(covid_ct_sum, file = "rda/covid_ct_sum.rda")

town_growth_top10 <- head(covid_ct_towns_sum[ind_new_case_town,],17)
save(town_growth_top10, file = "rda/town_growth_top10.rda")

county_growth_ord <- head(covid_ct_counties_sum[ind_new_case_county,],8)
save(county_growth_ord, file = "rda/county_growth_ord.rda")

# LAST UPDATE
ind_tdy

