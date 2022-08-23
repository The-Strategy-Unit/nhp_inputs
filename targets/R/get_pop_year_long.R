get_pop_year_long <- function(age_table) {
  url_fn <- function(...) {
    httr::modify_url(
      "https://www.ons.gov.uk/",
      path = "file",
      query = list(
        uri = paste(
          sep = "/",
          "",
          "peoplepopulationandcommunity",
          "populationandmigration",
          "populationestimates",
          "datasets",
          "lowersuperoutputareamidyearpopulationestimates",
          ...
        )
      )
    )
  }

  urls <- list(
    "202021" = url_fn("mid2020sape23dt2", "sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"),
    "201920" = url_fn("mid2019sape22dt2", "sape22dt2mid2019lsoasyoaestimatesunformatted.zip"),
    "201819" = url_fn("mid2018sape21dt1a", "sape21dt2mid2018lsoasyoaestimatesunformatted.zip"),
    "201718" = url_fn("mid2017", "sape20dt2mid2017lsoasyoaestimatesunformatted.zip"),
    "201617" = url_fn("mid2016", "sape20dt2mid2016lsoasyoaestimatesunformatted.zip"),
    "201516" = url_fn("mid2015", "sape20dt2mid2015lsoasyoaestimatesunformatted.zip"),
    "201415" = url_fn("mid2014", "sape20dt2mid2014lsoasyoaestimatesunformatted.zip")
  )

  load_sape_file <- function(url) {
    if (stringr::str_ends(url, ".zip")) {
      zip <- tempfile(fileext = ".zip")
      download.file(url, zip, mode = "wb")
      tf <- unzip(zip, list = TRUE)$Name[[1]] |>
        withr::local_file()
      unzip(zip, tf)
    } else {
      tf <- tempfile(fileext = ".xlsx")
      download.file(url, tf, mode = "wb")
    }

    readxl::excel_sheets(tf) |>
      stringr::str_subset("[Mm]ales$") |>
      purrr::set_names(\(.x) ifelse(stringr::str_detect(.x, "Females"), "2", "1")) |>
      purrr::map_dfr(readxl::read_excel, path = tf, skip = 4, .id = "sex") |>
      dplyr::select(.data$sex, lsoa11 = 2, tidyselect::matches("^\\d{1,2}\\+?$")) |>
      dplyr::filter(stringr::str_starts(.data$lsoa11, "E")) |>
      tidyr::pivot_longer(tidyselect::matches("^\\d"), names_to = "age", values_to = "pop") |>
      dplyr::inner_join(age_table, by = "age") |>
      dplyr::count(.data$sex, .data$age_grp, .data$lsoa11, wt = .data$pop, name = "pop")
  }

  urls |>
    purrr::map_dfr(load_sape_file, .id = "fyear") |>
    dplyr::mutate(dplyr::across(.data$fyear, as.numeric))
}
