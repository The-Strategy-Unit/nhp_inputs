get_age_table <- function() {
  tibble::tibble(age = 0:90) |>
    dplyr::mutate(
      age_group = cut_age(.data$age),
      dplyr::across("age", ~ paste0(.x, ifelse(.x == max(.x), "+", "")))
    )
}
