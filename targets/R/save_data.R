save_data <- function(...) {
  unlink("providers", TRUE, TRUE)

  list(...) |>
    purrr::imap(\(data, key) dplyr::group_nest(data, .data$procode, .data$strategy, .key = key)) |>
    purrr::reduce(dplyr::inner_join, by = c("procode", "strategy")) |>
    tidyr::pivot_longer(!where(rlang::is_atomic)) |>
    dplyr::filter(!purrr::map_lgl(.data$value, is.null)) |>
    dplyr::group_nest(.data$procode) |>
    purrr::pwalk(
      \(procode, data) {
        purrr::pwalk(
          data,
          \(strategy, name, value) {
            path <- paste("providers", procode, strategy, sep = "/")
            dir.create(path, showWarnings = FALSE, recursive = TRUE)

            value |>
              janitor::remove_empty(which = "cols") |>
              saveRDS(paste0(path, "/", name, ".rds"))
          }
        )
      }
    )
}
