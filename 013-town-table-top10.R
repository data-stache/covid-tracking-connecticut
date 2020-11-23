# LOAD DATA
load("rda/town_growth_top10.rda")


### CT TOWNS TABLE
TBL_CT_Town_7day <- town_growth_top10 %>%
  select(town,
         county,
         sum_new_cases,
         sum_new_tests,
         percent_pos,
         sum_new_deaths) %>%
  gt() %>% 
  ## LABELS STUB / TITLE / SUBTITLE
  tab_options(table.font.size = pct(75)) %>%
  tab_header(title = md("**TOP 10% OF TOWNS FOR COVID GROWTH IN CONNECTICUT**"),
             subtitle = md("*Based on Total Cases Over the Last Seven Days*")) %>%
  ## RENAMES COLUMNS
  cols_label(town = md("**Town**"),
             county = md("**County**"),
             sum_new_cases = md("**New Cases**"),                               
             sum_new_tests = md("**New Tests**"),
             percent_pos = md("**Percent Positive**"),
             sum_new_deaths = md("**New Deaths**")) %>%
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
            locations = cells_body(columns = vars(town))) %>% 
  ## STRIPING
  opt_row_striping(row_striping = TRUE) %>%                                      
  tab_source_note(source_note = md("*Created by Andrew F. Griffin, Covid Data from data.ct.gov*"))

TBL_CT_Town_7day
TBL_CT_Town_7day %>%
  gtsave("TBL_CT_Town_7day.html", path = "/Users/andrewgriffin/projects/covid-tracking-connecticut/figs")
