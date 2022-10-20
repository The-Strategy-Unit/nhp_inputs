save_data <- function(...) {
  unlink("providers", TRUE, TRUE)

  list(...) |>
    purrr::imap(\(data, key) dplyr::group_nest(data, .data$procode, .data$strategy, .key = key)) |>
    purrr::reduce(dplyr::inner_join, by = c("procode", "strategy")) |>
    tidyr::pivot_longer(!where(rlang::is_atomic)) |>
    dplyr::filter(!purrr::map_lgl(.data$value, is.null)) |>
    dplyr::mutate(dplyr::across("value", purrr::map, janitor::remove_empty, "cols")) |>
    dplyr::group_nest(.data$procode, .data$strategy) |>
    dplyr::mutate(dplyr::across("data", purrr::map, tibble::deframe)) |>
    dplyr::group_nest(.data$procode) |>
    dplyr::mutate(dplyr::across("data", purrr::map, tibble::deframe)) |>
    tibble::deframe() |>
    saveRDS("provider_data.Rds")

  list("save_data", Sys.time())
}
