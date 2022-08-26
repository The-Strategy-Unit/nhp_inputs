cut_age <- function(age, break_size = 5, upper_age = 90) {
  breaks <- seq(0, upper_age, break_size)

  label_fn <- function(.x) {
    if (.x == upper_age) {
      return(paste0(upper_age, "+"))
    }
    .x <- stringr::str_pad(.x + c(0, break_size - 1), 2, pad = "0")
    paste(.x, collapse = "-")
  }

  labels <- purrr::map_chr(breaks, label_fn)

  cut(age, c(breaks, Inf), labels, right = FALSE) |>
    as.character()
}
