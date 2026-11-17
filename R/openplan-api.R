create_openplan_api_request <- function(...) {
  uri <- envvars[["OPENPLAN_API_URI"]]
  app_id <- envvars[["OPENPLAN_API_APP_ID"]]

  token <- azkit::get_auth_token(
    resource = app_id,
    tenant = envvars[["AZ_TENANT_ID"]]
  ) |>
    AzureAuth::extract_jwt()

  httr2::request(uri) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_url_path(...)
}

get_peers <- function(provider) {
  create_openplan_api_request(
    "v1",
    "reference",
    "peers",
    "provider",
    provider
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
