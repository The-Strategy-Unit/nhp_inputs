#' Download Provider Data
#'
#' Download a selected type of provider data from ADLS and save it as a parquet
#' file in the app's data directory.
#'
#' @param file The name of the file to download.
#' @param data_path The path to the app's data directory.
#' @param inputs_data_version The version of the inputs data to use.
#' @param ... Additional arguments to pass to `AzureStor::download_adls_file()`.
#'
#' @return The path to the downloaded file.
download_provider_data <- function(
  file,
  data_path = app_sys("app", "data"),
  inputs_data_version = Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev"),
  ...
) {
  fs <- get_adls_fs()
  src <- glue::glue("{inputs_data_version}/provider/{file}.parquet")
  dest <- file.path(data_path, glue::glue("{file}.parquet"))

  AzureStor::download_adls_file(fs, src = src, dest = dest, ...)

  invisible(dest)
}

#' Get ADLS Filesystem
#'
#' Get the Azure Data Lake Storage filesystem using the endpoint and container
#' specified in environment variables. (AZ_STORAGE_EP and AZ_STORAGE_CONTAINER)
#'
#' @return the adls filesystem
get_adls_fs <- function() {
  token <- azkit::get_auth_token()

  Sys.getenv("AZ_STORAGE_EP") |>
    AzureStor::adls_endpoint(token = token) |>
    AzureStor::adls_filesystem(Sys.getenv("AZ_STORAGE_CONTAINER"))
}

#' Get All Data Files
#'
#' Download all provider data files from ADLS and save them as parquet files in
#' the app's data directory.
#'
#' @param inputs_data_version The version of the inputs data to use.
#' @return NULL
get_all_data_files <- function(
  inputs_data_version = Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev")
) {
  data_path <- file.path("inst", "app", "data")
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }

  files <- c(
    "rates",
    "age_sex",
    "diagnoses",
    "procedures",
    "baseline",
    "inequalities",
    "expat",
    "repat_local",
    "repat_nonlocal"
  )

  purrr::walk(
    purrr::set_names(files),
    download_provider_data,
    inputs_data_version = inputs_data_version,
    data_path = data_path,
    overwrite = TRUE
  )

  download_params_schema(
    data_path = data_path,
    app_version = inputs_data_version
  )

  invisible(NULL)
}
