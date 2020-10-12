# LOAD DATA
load("rda/covid_ct_counties_sum.rda")

### CT TABLE
TBL_CT_County_7day <- covid_ct_counties_sum %>%
  select(county,
         sum_cases_percap,
         sum_hosp_percap,
         sum_tests_percap,
         percent_pos,
         sum_deaths_percap) %>%
  gt() %>% 
  tab_options(table.font.size = pct(75)) %>%
  ## LABELS STUB / TITLE / SUBTITLE
  tab_header(title = md("**COVID GROWTH BY COUNTY IN CONNECTICUT**"),
             subtitle = md("*Based on Total Cases Per 100k Over the Last Seven Days*")) %>%
  ## RENAMES COLUMNS
  cols_label(county = md("**County**"), 
             sum_cases_percap = md("**New Cases**"),
             sum_hosp_percap = md("**New Hospitalization**"),
             sum_tests_percap = md("**New Tests**"),
             percent_pos = md("**Percent Positive**"),
             sum_deaths_percap = md("**New Deaths**")) %>%
  ## FORMATS DATA
  fmt_percent(columns = vars(percent_pos),
              decimals = 1) %>%
  cols_align(align = "center") %>%
  fmt_number(columns =  vars(sum_cases_percap,
                             sum_tests_percap,
                             sum_deaths_percap),
             decimals = 0) %>%
  ## BOLD STATES COLUMN
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = vars(county))) %>%   
  ## STRIPING
  opt_row_striping(row_striping = TRUE) %>%
  tab_source_note(source_note = md("*Created by Andrew F. Griffin, Covid Data from data.ct.gov*"))

TBL_CT_County_7day
TBL_CT_County_7day %>%
  gtsave("TBL_CT_County_7day.html", path = "/Users/andrewgriffin/projects/covid-tracking-connecticut/figs")
