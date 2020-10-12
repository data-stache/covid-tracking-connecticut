library(RSocrata)

# TOWN TESTS / CASES / DEATHS
url <- "https://data.ct.gov/resource/28fr-iqnx.csv"
dest_file <- "data/covid-daily-town-report-connecticut.csv"
file_a <- read.socrata(url)
write.csv(file_a, dest_file, row.names = FALSE)

# COUNTY HOSPITALIZATIONS
url <- "https://data.ct.gov/resource/bfnu-rgqt.csv"
dest_file <- "data/covid-daily-county-report-connecticut.csv"
file_b <- read.socrata(url)
write.csv(file_b, dest_file, row.names = FALSE)



