get_lkp_provider_names2 <- function() {
  # pre-allocate a vector to store up to 100 pages of data, should be sufficient
  results <- vector("list", 100)

  page <- 1

  url <- httr::modify_url(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations",
    query = list(
      PrimaryRoleId = "RO197",
      Limit = 500
    )
  )

  repeat {
    req <- httr::GET(url)

    stopifnot("API call failed" = httr::status_code(req) == 200)

    results[page] <- httr::content(req, type = "application/json")

    if (is.null(req$headers$`next-page`)) {
      break
    }

    page <- page + 1
    url <- req$headers$`next-page`
  }

  dplyr::bind_rows(results[1:page]) |>
    dplyr::select(procode = "OrgId", trust_name = "Name")
}
