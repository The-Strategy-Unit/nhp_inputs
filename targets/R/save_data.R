save_data <- function(nhp_current_cohort, ...) {
  unlink("providers", TRUE, TRUE)

  provider_data <- list(...) |>
    purrr::imap(\(data, key) dplyr::group_nest(data, .data$procode, .data$strategy, .key = key)) |>
    purrr::reduce(dplyr::inner_join, by = c("procode", "strategy")) |>
    tidyr::pivot_longer(!where(rlang::is_atomic)) |>
    dplyr::filter(!purrr::map_lgl(.data$value, is.null)) |>
    dplyr::mutate(dplyr::across("value", purrr::map, janitor::remove_empty, "cols")) |>
    dplyr::group_nest(.data$procode, .data$strategy) |>
    dplyr::mutate(dplyr::across("data", purrr::map, tibble::deframe)) |>
    dplyr::group_nest(.data$procode) |>
    dplyr::mutate(dplyr::across("data", purrr::map, tibble::deframe)) |>
    dplyr::filter(.data$procode %in% nhp_current_cohort) |>
    tibble::deframe()

  available_strategies <- provider_data |>
    purrr::imap(\(.x, .i) {
      .x |>
        purrr::map("rates") |>
        purrr::map(dplyr::filter, .data$peer == .i) |>
        purrr::map_dfr(dplyr::select, "fyear", .id = "strategy") |>
        dplyr::group_by(dplyr::across("fyear", as.character)) |>
        dplyr::summarise(dplyr::across("strategy", list), .groups = "drop") |>
        tibble::deframe()
    })

  saveRDS(provider_data, "provider_data.Rds")
  saveRDS(available_strategies, "available_strategies.Rds")

  list("save_data", Sys.time())
}
