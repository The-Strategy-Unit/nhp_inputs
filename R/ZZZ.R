#' @importFrom zeallot %<-%
#' @importFrom rlang .data .env `!!!`
#' @importFrom promises %...>% %...!%
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

md_file_to_html <- function(...) {
  file <- app_sys(...)

  if (!file.exists(file)) {
    return(NULL)
  }

  shiny::HTML(markdown::mark_html(file, output = FALSE, template = FALSE))
}

load_params <- function(file) {
  p <- jsonlite::read_json(file, simplifyVector = TRUE)

  # handle old non-demographic adjusment
  if (!is.null(p[["non-demographic"]][["elective"]])) {
    p[["non-demographic_adjustment"]] <- list(
      ip = list(),
      op = list(),
      aae = list()
    )
  }

  p
}

params_path <- function(user, dataset) {
  path <- file.path(
    Sys.getenv("PARAMS_DATA_PATH", "."),
    "params",
    user %||% ".",
    dataset
  )

  dir.create(path, FALSE, TRUE)

  path
}

params_filename <- function(user, dataset, scenario) {
  file.path(
    params_path(user, dataset),
    paste0(scenario, ".json")
  )
}
