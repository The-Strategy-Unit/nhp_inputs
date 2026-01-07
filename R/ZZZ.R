#' @importFrom zeallot %<-%
#' @importFrom rlang .data .env `!!!`
#' @importFrom promises %...>% %...!%
NULL

utils::globalVariables(c(
  "where", # source: https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846
  ".data",
  ".env"
))

#' Get RTT specialties data
#'
#' Reads the RTT specialties CSV file from the app data directory.
#'
#' @return A tibble containing RTT specialty codes and names.
#' @noRd
rtt_specialties <- function() {
  app_sys("app", "data", "rtt_specialties.csv") |>
    readr::read_csv(col_types = "cc")
}

#' Sanitize input names
#'
#' Converts input names to a standardized format by converting to lowercase,
#' replacing whitespace/underscores with hyphens, and removing invalid characters.
#'
#' @param .x A character vector of input names to sanitize.
#'
#' @return A character vector of sanitized input names.
#' @noRd
sanitize_input_name <- function(.x) {
  .x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("(\\s|\\_|-)+", "-") |>
    stringr::str_remove_all("[^a-z0-9-]+")
}

# suppress vs code / languageserver "no visible binding" warnings
if (FALSE) {
  .data <- NULL
  .env <- NULL
}

#' Convert markdown file to HTML
#'
#' Reads a markdown file from the app directory and converts it to HTML.
#'
#' @param ... Character vectors specifying subdirectory and file(s) within the
#'   app package. Passed to \code{app_sys()}.
#'
#' @return An HTML object containing the rendered markdown, or NULL if the file
#'   doesn't exist.
#' @noRd
md_file_to_html <- function(...) {
  file <- app_sys(...)

  if (!file.exists(file)) {
    return(NULL)
  }

  shiny::HTML(markdown::mark_html(file, output = FALSE, template = FALSE))
}

#' Load parameters from JSON file
#'
#' Reads model parameters from a JSON file and handles backward compatibility
#' for old parameter structures.
#'
#' @param file Path to the JSON file containing parameters.
#'
#' @return A list containing the model parameters.
#' @noRd
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

#' Get parameters directory path
#'
#' Constructs the file path to the parameters directory for a given user and
#' dataset, creating the directory if it doesn't exist.
#'
#' @param user Username or NULL for default.
#' @param dataset Dataset identifier.
#'
#' @return Character string containing the path to the parameters directory.
#' @noRd
params_path <- function(user, dataset) {
  path <- file.path(
    get_golem_config("params_data_path"),
    "params",
    user %||% ".",
    dataset
  )

  dir.create(path, FALSE, TRUE)

  path
}

#' Get parameters filename
#'
#' Constructs the full file path for a scenario parameters file.
#'
#' @param user Username or NULL for default.
#' @param dataset Dataset identifier.
#' @param scenario Scenario name.
#'
#' @return Character string containing the full path to the parameters file.
#' @noRd
params_filename <- function(user, dataset, scenario) {
  file.path(
    params_path(user, dataset),
    paste0(scenario, ".json")
  )
}

#' Check if app is running locally
#'
#' Determines whether the app is running in a local development environment
#' or in production based on environment variables.
#'
#' @return Logical indicating if the app is running locally (TRUE) or in
#'   production (FALSE).
#' @noRd
is_local <- function() {
  Sys.getenv("SHINY_PORT") == "" || !getOption("golem.app.prod", TRUE)
}

#' Encrypt a filename
#'
#' Encrypts a filename using AES-CBC encryption with HMAC for integrity.
#'
#' @param filename Character string containing the filename to encrypt.
#' @param key_b64 Base64-encoded encryption key. Defaults to the
#'   NHP_ENCRYPT_KEY environment variable.
#'
#' @return Base64-encoded string containing the encrypted filename with HMAC.
#' @noRd
encrypt_filename <- function(
  filename,
  key_b64 = Sys.getenv("NHP_ENCRYPT_KEY")
) {
  key <- openssl::base64_decode(key_b64)

  f <- charToRaw(filename)

  ct <- openssl::aes_cbc_encrypt(f, key, NULL)
  hm <- as.raw(openssl::sha256(ct, key))

  openssl::base64_encode(c(hm, ct))
}

#' Get parameters schema text
#'
#' Downloads and reads the JSON schema for model parameters from the GitHub
#' Pages site.
#'
#' @param app_version Version of the app to get the schema for. Defaults to the
#'   INPUTS_DATA_VERSION environment variable or "dev" if not set.
#'
#' @return Character string containing the JSON schema text.
#' @noRd
get_params_schema_text <- function(
  app_version = Sys.getenv("INPUTS_DATA_VERSION", "dev")
) {
  tf <- tempfile()

  utils::download.file(
    glue::glue(
      "https://the-strategy-unit.github.io/nhp_model/{app_version}/params-schema.json"
    ),
    tf
  )
  # append a newline to the end of the params-schema file, otherwise you get a warning
  # "incomplete final line found"
  cat("\n", file = tf, append = TRUE)

  paste(readLines(tf), collapse = "\n")
}

#' Create parameters schema object
#'
#' Creates a JSON schema validator object from schema text.
#'
#' @param schema_text Character string containing the JSON schema definition.
#'
#' @return A jsonvalidate json_schema object.
#' @noRd
create_params_schema <- function(schema_text) {
  jsonvalidate::json_schema$new(schema_text)
}
