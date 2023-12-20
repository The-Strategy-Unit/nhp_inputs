get_baseline_data <- function(all_providers, ...) {
  dplyr::bind_rows(..., .id = "activity_type") |>
    dplyr::ungroup() |>
    dplyr::filter(.data[["procode3"]] %in% all_providers) |>
    tidyr::nest(.by = c("procode3", "fyear")) |>
    tidyr::nest(.by = c("procode3")) |>
    tibble::deframe() |>
    purrr::map(tibble::deframe)
}
