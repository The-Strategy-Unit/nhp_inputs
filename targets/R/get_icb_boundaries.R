get_icb_boundaries <- function(icb_code_lu_path) {
  icbs <- readr::read_csv(icb_code_lu_path, col_types = "cc")

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
    dplyr::inner_join(icbs, by = "icb22cd")
}
