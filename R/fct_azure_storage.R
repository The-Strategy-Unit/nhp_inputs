#' azure_storage
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
load_rds_from_adls <- function(file) {
  fs <- get_adls_fs()
  AzureStor::storage_load_rds(fs, file)
}

#' Get ADLS Filesystem
#'
#' try to use a managed token, this will only work if run in an Azure data centre
#' if it fails, try instead to use Azure Resource Manager
#'
#' @return the adls filesystem
get_adls_fs <- function() {
  sa_name <- Sys.getenv("AZ_STORAGE_ACCOUNT")

  ep <- tryCatch(
    {
      token <- AzureAuth::get_managed_token("https://storage.azure.com/") |>
        AzureAuth::extract_jwt()

      AzureStor::adls_endpoint(
        glue::glue("https://{sa_name}.dfs.core.windows.net/"),
        token = token
      )
    },
    error = \(...) {
      AzureStor::adls_endpoint(
        endpoint = Sys.getenv("TARGETS_AZURE_SA_EP"),
        key = Sys.getenv("TARGETS_AZURE_SA_key")
      )
    }
  )
  AzureStor::adls_filesystem(ep, Sys.getenv("AZ_STORAGE_CONTAINER"))
}
