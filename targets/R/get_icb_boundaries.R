get_icb_boundaries <- function(icb_code_lu_path) {
  icbs <- readr::read_csv(icb_code_lu_path, col_types = "cc")

  icb_boundaries <- paste(
    sep = "/",
    "https://services1.arcgis.com",
    "ESMARspQHYMw9BZ9",
    "arcgis",
    "rest",
    "services",
    "Integrated_Care_Boards_July_2022_EN_BUC_2022",
    "FeatureServer",
    "0",
    "query?outFields=*&where=1%3D1&f=geojson"
  ) |>
    sf::read_sf() |>
    janitor::clean_names() |>
    dplyr::select(tidyselect::starts_with("icb22"))

  icb_boundaries |>
    dplyr::inner_join(icbs, by = "icb22cd")
}
