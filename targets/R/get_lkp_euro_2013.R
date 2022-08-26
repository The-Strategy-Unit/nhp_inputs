get_lkp_euro_2013 <- function() {
  # # FROM: https://www.opendata.nhs.scot/dataset/standard-populations/resource/29ce4cda-a831-40f4-af24-636196e05c1a
  readr::read_csv(
    "https://www.opendata.nhs.scot/datastore/dump/29ce4cda-a831-40f4-af24-636196e05c1a?bom=True",
    col_types = "_ccd"
  ) |>
    dplyr::rename(
      pop_euro = .data$EuropeanStandardPopulation,
      age_group = .data$AgeGroup,
      sex = .data$Sex
    ) |>
    dplyr::mutate(
      dplyr::across(
        .data$age_group,
        purrr::compose(
          .dir = "forward",
          purrr::partial(stringr::str_remove, pattern = " years"),
          purrr::partial(stringr::str_replace, pattern = "^(\\d)-(\\d)$", replacement = "0\\1-0\\2"),
          purrr::partial(stringr::str_replace, pattern = "plus", replacement = "+")
        )
      ),
      across(sex, ~ ifelse(.x == "Male", "1", "2"))
    )
}
