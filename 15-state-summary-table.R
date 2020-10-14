# LOAD DATA
load("rda/covid_ct.rda")
load("rda/ind_tylm_date.rda")

### CT TABLE
TBL_CT_State_Summary <- covid_ct %>%
  filter(date %in% ind_tylm_date) %>%
  select(date, new_cases, new_tests, new_deaths, current_hosp, new_hosp, percent_pos) %>%
  gt() %>% 
  tab_options(table.font.size = pct(75)) %>%
  ## LABELS STUB / TITLE / SUBTITLE
  tab_header(title = md("**COVID GROWTH IN CONNECTICUT**"),
             subtitle = md("*Today / Yesterday / Last Week / Last Month*")) %>%
  ## RENAMES COLUMNS
  cols_label(date = md("**Date**"), 
             new_cases = md("**New Cases**"),
             new_tests = md("**New Tests**"),
             new_deaths = md("**New Deaths**"),
             current_hosp = md("**Currently Hospitalized**"),
             new_hosp = md("**Newly Hospitalized**"),
             percent_pos = md("**Percent Positive**")) %>%
  ## FORMATS DATA
  fmt_percent(columns = vars(percent_pos),
              decimals = 1) %>%
  cols_align(align = "center") %>%
  fmt_number(columns =  vars(new_cases,
                             new_tests,
                             new_deaths),
             decimals = 0) %>%
  ## BOLD STATES COLUMN
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = vars(date))) %>%   
  ## STRIPING
  opt_row_striping(row_striping = TRUE) %>%
  tab_source_note(source_note = md("*Created by Andrew F. Griffin, Covid Data from data.ct.gov*"))

TBL_CT_State_Summary
TBL_CT_State_Summary %>%
  gtsave("TBL_CT_State_Summary.html", path = "/Users/andrewgriffin/projects/covid-tracking-connecticut/figs")
