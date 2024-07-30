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

  load_sape_file_before_2021 <- function(url) {
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
      dplyr::select("sex", lsoa11 = 2, tidyselect::matches("^\\d{1,2}\\+?$")) |>
      dplyr::filter(stringr::str_starts(.data$lsoa11, "E")) |>
      tidyr::pivot_longer(tidyselect::matches("^\\d"), names_to = "age", values_to = "pop") |>
      dplyr::inner_join(age_table, by = "age") |>
      dplyr::count(.data$sex, .data$age_group, .data$lsoa11, wt = .data$pop, name = "pop")
  }

  load_sape_file_2021_2022 <- function(url) {
    tf <- tempfile(fileext = ".xlsx")
    download.file(url, tf, mode = "wb")

    lsoa_lookup <- httr::modify_url(
      "https://opendata.arcgis.com",
      path = c(
        "api",
        "v3",
        "datasets",
        "e99a92fb7607495689f2eeeab8108fd6_0",
        "downloads",
        "data"
      ),
      query = list(
        format = "csv",
        spatialRefId = "4326",
        where = "1=1"
      )
    ) |>
      readr::read_csv() |>
      janitor::clean_names() |>
      dplyr::select(tidyselect::all_of(tidyselect::matches("lsoa\\d+cd"))) |>
      dplyr::mutate(fraction = 1 / dplyr::n(), .by = "lsoa21cd")

    purrr::map(
      list(
        "202122" = "Mid-2021 LSOA 2021",
        "202223" = "Mid-2022 LSOA 2021"
      ),
      readxl::read_xlsx,
      path = tf, skip = 3
    ) |>
      dplyr::bind_rows(.id = "fyear") |>
      dplyr::select(
        "fyear",
        lsoa21cd = "LSOA 2021 Code",
        tidyselect::all_of(tidyselect::matches("^[FM]\\d+$"))
      ) |>
      tidyr::pivot_longer(-c(.data[["fyear"]], .data[["lsoa21cd"]])) |>
      dplyr::mutate(
        sex = ifelse(.data[["name"]] |> stringr::str_starts("F"), "2", "1"),
        age = stringr::str_sub(.data[["name"]], 2)
      ) |>
      dplyr::inner_join(
        age_table,
        by = dplyr::join_by("age")
      ) |>
      dplyr::inner_join(
        lsoa_lookup,
        by = dplyr::join_by("lsoa21cd"),
        relationship = "many-to-many"
      ) |>
      dplyr::group_by(
        .data[["fyear"]],
        .data[["sex"]],
        .data[["age_group"]],
        lsoa11 = .data[["lsoa11cd"]]
      ) |>
      dplyr::summarise(
        pop = sum(.data[["value"]] * .data[["fraction"]])
      )
  }

  before_2021 <- urls |>
    purrr::map_dfr(load_sape_file_before_2021, .id = "fyear")

  after_2021 <- load_sape_file_2021_2022(url_fn("mid2021andmid2022", "sapelsoasyoatablefinal.xlsx"))

  dplyr::bind_rows(before_2021, after_2021) |>
    dplyr::mutate(dplyr::across("fyear", as.numeric))
}
