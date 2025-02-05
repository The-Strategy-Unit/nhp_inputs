#' azure_storage
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
load_rds_from_adls <- function(file, inputs_data_version = Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev")) {
  fs <- get_adls_fs()
  AzureStor::storage_load_rds(fs, glue::glue("{inputs_data_version}/{file}"))
}

#' Get Provider Data
#'
#' Read the parquet file containing a selected tupe of provider data.
#'
#' @return A tibble.
load_provider_data <- function(
    file,
    inputs_data_version = Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev")
) {
  fs <- get_adls_fs()
  fs |>
    AzureStor::download_adls_file(
      glue::glue("{inputs_data_version}/{file}.parquet"),
      dest = NULL
    ) |>
    arrow::read_parquet() |>
    tibble::as_tibble()
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
