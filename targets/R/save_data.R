save_data <- function(...) {
  list(...) |>
    purrr::imap(\(data, key) dplyr::group_nest(data, .data$procode, .data$strategy, .key = key)) |>
    purrr::reduce(dplyr::full_join, by = c("procode", "strategy")) |>
    # make sure we have rows for the age_sex and diagnoses
    dplyr::rowwise() |>
    dplyr::filter(!(is.null(.data$age_sex) & is.null(.data$diagnoses))) |>
    dplyr::ungroup() |>
    #
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
