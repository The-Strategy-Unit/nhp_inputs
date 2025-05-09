### code to prepare `nee` dataset goes here

# This is centiles - not needed now but retained for completeness

nee_lookup <- readr::read_csv("data-raw/NEE_lookup.csv")

# The table with p10/ 90 values and mean
readr::read_csv("data-raw/NEE_table.csv", col_types = "cccccddddddd") |>
  # Flip the results to fit in with the rest of the dashboards - with 100%
  # representing no change and 0% representing total elimination.
  dplyr::mutate(
    dplyr::across(c("mean", "percentile10", "percentile90"), \(x) 100 - x)
  ) |>
  # Only keep the columns currently used in the app (4 of 12)
  dplyr::select(c("param_name", "mean", "percentile10", "percentile90")) |>
  readr::write_csv("inst/app/data/nee_table.csv")
