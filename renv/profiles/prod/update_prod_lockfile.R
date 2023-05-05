renv::dependencies() |>
  tibble::as_tibble() |>
  dplyr::filter(Source |> stringr::str_detect("targets", TRUE)) |>
  dplyr::pull(Package) |>
  unique() |>
  renv::snapshot(packages = _)
