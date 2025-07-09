#' @importFrom zeallot %<-%
#' @importFrom rlang .data .env `!!!`
#' @importFrom promises %...>% %...!%
NULL

utils::globalVariables(c(
  "where", # source: https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846
  ".data",
  ".env"
))

rtt_specialties <- function() {
  app_sys("app", "data", "rtt_specialties.csv") |>
    readr::read_csv(col_types = "cc")
}

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
    get_golem_config("params_data_path"),
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

# check to see whether the app is running locally or in production
is_local <- function() {
  Sys.getenv("SHINY_PORT") == "" || !getOption("golem.app.prod", TRUE)
}

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

get_params_schema_text <- function(
  app_version = Sys.getenv("INPUTS_DATA_VERSION", "dev")
) {
  tf <- tempfile()

  download.file(
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

create_params_schema <- function(schema_text) {
  jsonvalidate::json_schema$new(schema_text)
}
