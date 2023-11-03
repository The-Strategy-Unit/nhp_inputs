mod_theatres_load_specialties <- function() {
  app_sys("app", "data", "theatre_specialties.Rds") |>
    readRDS() |>
    dplyr::mutate(
      sanitized_code = sanitize_input_name(.data$code)
    )
}

mod_theatres_ui_table <- function(ns = identity) {
  input_fn <- function(type, code, value, fn, desc) {
    purrr::map(
      code,
      ~ fn(
        ns(glue::glue("{type}_{desc}_{.x}")),
        label = NULL,
        value = value,
        min = 0,
        max = 10,
        step = 0.1
      ) |>
        as.character() |>
        gt::html()
    )
  }

  baseline_fn <- \(type) \(code) input_fn(type, code, 1, shiny::numericInput, "baseline")
  slider_fn <- \(type) \(code) input_fn(type, code, c(1, 2), shiny::sliderInput, "param")

  mod_theatres_load_specialties() |>
    dplyr::mutate(
      dplyr::across(
        "sanitized_code",
        .names = "{.fn}",
        c(
          spells_baseline = baseline_fn("spells"),
          spells_param = slider_fn("spells"),
          cases_baseline = baseline_fn("cases"),
          cases_param = slider_fn("cases")
        )
      )
    ) |>
    dplyr::select(-tidyselect::ends_with("code")) |>
    gt::gt(rowname_col = "specialty") |>
    gt::tab_spanner(
      label = "Spells Per Case",
      columns = tidyselect::starts_with("spells")
    ) |>
    gt::tab_spanner(
      label = "Cases Per 4 Hour Session",
      columns = tidyselect::starts_with("cases")
    ) |>
    gt::cols_label(
      spells_baseline = "Baseline",
      spells_param = "Model Year",
      cases_baseline = "Baseline",
      cases_param = "Model Year"
    ) |>
    gt::cols_width(
      specialty ~ gt::pct(20),
      tidyselect::ends_with("baseline") ~ gt::pct(10),
      tidyselect::ends_with("param") ~ gt::pct(25)
    ) |>
    gt::tab_options(table.width = gt::pct(100))
}
