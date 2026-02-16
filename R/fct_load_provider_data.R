#' Get Provider Data
#'
#' Read the parquet file containing a selected type of provider data.
#'
#' @param file The name of the file to read.
#' @param inputs_data_version The version of the inputs data to use.
#' @return A tibble.
load_provider_data <- function(file, data_path = app_sys("app", "data")) {
  file.path(data_path, glue::glue("{file}.parquet")) |>
    arrow::read_parquet() |>
    tibble::as_tibble()
}

get_rates_data <- function() {
  rates <- load_provider_data("rates") |>
    dplyr::select(-"crude_rate") |>
    dplyr::rename(rate = "std_rate")

  national_rate <- rates |>
    dplyr::filter(
      .data$provider == "national"
    ) |>
    dplyr::summarise(
      .by = c("fyear", "strategy"),
      national_rate = dplyr::first(.data$rate)
    )

  rates |>
    dplyr::filter(.data$provider != "national") |>
    dplyr::inner_join(national_rate, by = c("fyear", "strategy"))
}

get_age_sex_data <- function() {
  age_sex <- load_provider_data("age_sex")

  age_fct <- sort(unique(age_sex[["age_group"]]))

  age_sex |>
    dplyr::mutate(
      dplyr::across("sex", as.character),
      age_group = factor(
        .data[["age_group"]],
        levels = .env[["age_fct"]]
      )
    )
}

get_diagnoses_data <- function() {
  load_provider_data("diagnoses")
}

get_procedures_data <- function() {
  load_provider_data("procedures")
}

get_baseline_data <- function() {
  load_provider_data("baseline")
}

get_wli_data <- function() {
  load_provider_data("wli")
}

get_inequalities_data <- function() {
  load_provider_data("inequalities")
}

get_expat_data <- function() {
  load_provider_data("expat")
}

get_repat_local_data <- function() {
  load_provider_data("repat_local")
}

get_repat_nonlocal_data <- function() {
  load_provider_data("repat_nonlocal")
}
