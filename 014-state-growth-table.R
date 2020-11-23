# LOAD DATA
load("rda/covid_ct_sum.rda")

### CT TABLE
TBL_CT_State_7day <- covid_ct_sum %>%
  gt() %>% 
  tab_options(table.font.size = pct(75)) %>%
  ## LABELS STUB / TITLE / SUBTITLE
  tab_header(title = md("**COVID GROWTH IN CONNECTICUT**"),
             subtitle = md("*Last Seven Days Compared to Last Week*")) %>%
  ## RENAMES COLUMNS
  cols_label(week = md("**Period**"), 
             sum_new_cases = md("**New Cases**"),
             sum_new_tests = md("**New Tests**"),
             sum_new_deaths = md("**New Deaths**"),
             m_hosp = md("**Avg Hospitalized**"),
             sum_new_hosp = md("**New Hospitalizations**"),
             percent_pos = md("**Percent Positive**")) %>%
  ## FORMATS DATA
  fmt_percent(columns = vars(percent_pos),
              decimals = 1) %>%
  cols_align(align = "center") %>%
  fmt_number(columns =  vars(sum_new_cases,
                             sum_new_tests,
                             sum_new_deaths),
             decimals = 0) %>%
  ## BOLD STATES COLUMN
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = vars(week))) %>%   
  ## STRIPING
  opt_row_striping(row_striping = TRUE) %>%
  tab_source_note(source_note = md("*Created by Andrew F. Griffin, Covid Data from data.ct.gov*"))

TBL_CT_State_7day
TBL_CT_State_7day %>%
  gtsave("TBL_CT_State_7day.html", path = "/Users/andrewgriffin/projects/covid-tracking-connecticut/figs")
