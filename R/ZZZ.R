#' @importFrom zeallot %<-%
#' @importFrom rlang .data .env
#' @importFrom promises %...>%
NULL

utils::globalVariables(c(
  "where", # source: https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846
  ".data", ".env"
))

rtt_specialties <- function() {
  readRDS(app_sys("app", "data", "rtt_specialties.Rds")) |>
    tibble::enframe("specialty", "code")
}

sanitize_input_name <- \(.x) .x |>
  stringr::str_to_lower() |>
  stringr::str_replace_all("(\\s|\\_|-)+", "-") |>
  stringr::str_remove_all("[^a-z0-9-]+")

# suppress vs code / languageserver "no visible binding" warnings
if (FALSE) {
  .data <- NULL
}
