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
  ep_uri <- Sys.getenv("AZ_STORAGE_EP")

  resource <- "https://storage.azure.com/"
  token <- if (imds_available()) {
    AzureAuth::get_managed_token(resource)
  } else {
    AzureAuth::get_azure_token(
      resource = resource,
      tenant = "common",
      app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
      auth_type = "authorization_code"
    )
  }

  ep <- AzureStor::adls_endpoint(ep_uri, token = token)

  AzureStor::adls_filesystem(ep, Sys.getenv("AZ_STORAGE_CONTAINER"))
}

#' Check if Azure Instance Metadata Service (IMDS) is Available
#'
#' This function checks if the Azure Instance Metadata Service (IMDS) is
#' available by attempting to make a request to the IMDS endpoint. The result is
#' cached in an environment variable for future use, saving the need for
#' repeated checks.
#'
#' You can also set the `IMDS_AVAILABLE` environment variable manually to
#' "true" or "false" to override the automatic check, which can be useful for
#' testing or in environments where the check may not work correctly.
imds_available <- function() {
  available <- Sys.getenv("IMDS_AVAILABLE")

  if (available == "") {
    imds_available <- tryCatch(
      {
        Sys.getenv("MSI_ENDPOINT", "http://169.254.169.254/metadata/identity/oauth2") |>
          httr2::request() |>
          httr2::req_headers(Metadata = "true") |>
          httr2::req_timeout(0.2) |>
          httr2::req_perform()

        TRUE
      },
      error = function(e) FALSE
    )
    Sys.setenv(IMDS_AVAILABLE = imds_available)
  }
  as.logical(imds_available)
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
