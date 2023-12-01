### code to prepare `nee` dataset goes here

# this is centiles- not needed now but included for completeness

nee_lookup <- readr::read_csv("data-raw/NEE_lookup.csv")

# the table with p10/ 90 values and mean

nee_table <- readr::read_csv("data-raw/NEE_table.csv")

usethis::use_data(nee_lookup, overwrite = TRUE)
