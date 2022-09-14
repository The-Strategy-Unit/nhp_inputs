get_provider_locations <- function(providers) {
  trusts <- NHSRtools::ods_get_trusts() |>
    dplyr::filter(.data$org_id %in% providers) |>
    dplyr::select(.data$org_id, .data$name, .data$post_code)

  postcodes <- unique(trusts$post_code)
  n_postcodes <- length(postcodes)
  postcodes_results <- vector("list", n_postcodes)

  for (i in seq(1, n_postcodes, 100)) {
    j <- min(n_postcodes, i + 99)
    postcodes_results[i:j] <- PostcodesioR::bulk_postcode_lookup(
      list(
        postcodes = stringr::str_replace_all(postcodes[i:j], "\\s", "")
      )
    )
  }

  postcodes_results |>
    purrr::map("result") |>
    purrr::map(\(.x) sf::st_point(c(.x$longitude, .x$latitude))) |>
    sf::st_as_sfc(crs = 4326) |>
    sf::st_as_sf() |>
    dplyr::rename(geometry = .data$x) |>
    dplyr::mutate(post_code = postcodes, .before = .data$geometry) |>
    dplyr::inner_join(trusts, by = c("post_code"))
}
