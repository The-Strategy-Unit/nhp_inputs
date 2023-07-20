renv::dependencies() |>
  tibble::as_tibble() |>
  dplyr::filter(Source |> stringr::str_detect("targets", TRUE)) |>
  dplyr::pull(Package) |>
  c(
    "mockery", "rcmdcheck", "spelling", "testthat"
  ) |>
  unique() |>
  renv::snapshot(packages = _)
