get_icb_boundaries <- function() {
  icbs <- httr::modify_url(
    "https://opendata.arcgis.com",
    path = c(
      "api",
      "v3",
      "datasets",
      "6924fa126d7a428782eda4b374b6d734_0",
      "downloads",
      "data"
    ),
    query = list(
      format = "csv",
      spatialRefId = "4326",
      where = "1=1"
    )
  ) |>
    readr::read_csv(col_types = "cc__") |>
    # bodge rename from the 23 year to 22
    dplyr::select(
      icb22cd = 1,
      icb22cdh = 2
    )

  icb_boundaries <- httr::modify_url(
    "https://services1.arcgis.com",
    path = c(
      "ESMARspQHYMw9BZ9",
      "arcgis",
      "rest",
      "services",
      "ICB_JUL_2022_EN_BUC_V3",
      "FeatureServer",
      "0",
      "query"
    ),
    query = list(
      outFields = "*",
      where = "1=1",
      f = "geojson"
    )
  ) |>
    sf::read_sf() |>
    janitor::clean_names() |>
    dplyr::select(tidyselect::starts_with("icb22"))

  icb_boundaries |>
    dplyr::inner_join(icbs, by = "icb22cd") |>
    dplyr::relocate("icb22cdh", .before = "icb22cd")
}
