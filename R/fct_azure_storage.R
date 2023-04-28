#' azure_storage
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
load_rds_from_azure <- function(file) {
  ep <- AzureStor::adls_endpoint(
    endpoint = Sys.getenv("TARGETS_AZURE_SA_EP"),
    key = Sys.getenv("TARGETS_AZURE_SA_key")
  )
  fs <- AzureStor::adls_filesystem(ep, "inputs-data")

  fn <- withr::local_tempfile(fileext = ".rds")

  AzureStor::download_adls_file(fs, file, fn)

  readRDS(fn)
}
