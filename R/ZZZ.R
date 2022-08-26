#' @importFrom zeallot %<-%
#' @importFrom rlang .data .env
NULL

utils::globalVariables(c(
  "where", # source: https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846
  ".data", ".env"
))
