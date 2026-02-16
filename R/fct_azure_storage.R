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
#' try to use a managed token, this will only work if run in an Azure data centre
#' if it fails, try instead to use Azure Resource Manager
#'
#' @return the adls filesystem
get_adls_fs <- function() {
  ep_uri <- Sys.getenv("LOCAL_STORAGE_EP")

  ep <- if (ep_uri != "") {
    sa_key <- Sys.getenv("LOCAL_STORAGE_KEY")

    AzureStor::adls_endpoint(ep_uri, key = sa_key)
  } else {
    ep_uri <- Sys.getenv("AZ_STORAGE_EP")
    token <- AzureAuth::get_managed_token("https://storage.azure.com/") |>
      AzureAuth::extract_jwt()

    AzureStor::adls_endpoint(ep_uri, token = token)
  }
  AzureStor::adls_filesystem(ep, Sys.getenv("AZ_STORAGE_CONTAINER"))
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
