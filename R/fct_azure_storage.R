#' Get Provider Data
#'
#' Read the parquet file containing a selected type of provider data.
#'
#' @param file The name of the file to read.
#' @param inputs_data_version The version of the inputs data to use.
#' @return A tibble.
load_provider_data <- function(
  file,
  inputs_data_version = Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev")
) {
  fs <- get_adls_fs()
  fs |>
    AzureStor::download_adls_file(
      glue::glue("{inputs_data_version}/provider/{file}.parquet"),
      dest = NULL
    ) |>
    arrow::read_parquet() |>
    tibble::as_tibble()
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
