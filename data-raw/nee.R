### code to prepare `nee` dataset goes here

# this is centiles- not needed now but included for completeness

nee_lookup <- readr::read_csv("data-raw/NEE_lookup.csv")

# the table with p10/ 90 values and mean

nee_table <- readr::read_csv("data-raw/NEE_table.csv")

# flip the results to fit in with the rest of the dashboards- with 100%
# representing no change and 0% representing total elimination

nee_table <- nee_table |>
  dplyr::mutate(dplyr::across(c(mean, percentile10, percentile90), ~ 100 - .x))

usethis::use_data(nee_table, overwrite = TRUE)
