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

#' theatres UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_theatres_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    title = "Theatres Utilisation",
    width = 12,
    gt::as_raw_html(mod_theatres_ui_table(ns), FALSE)
  )
}

#' theatres Server Functions
#'
#' @noRd
mod_theatres_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    mod_theatres_load_specialties() |>
      purrr::pwalk(\(code, sanitized_code, ...) {
        spells_baseline_id <- glue::glue("spells_baseline_{sanitized_code}")
        spells_param_id <- glue::glue("spells_param_{sanitized_code}")
        cases_baseline_id <- glue::glue("cases_baseline_{sanitized_code}")
        cases_param_id <- glue::glue("cases_param_{sanitized_code}")

        shiny::observe({
          s <- input[[spells_baseline_id]]
          c <- input[[cases_baseline_id]]

          params$theatres$spells_per_case[[code]]$baseline <- s
          params$theatres$cases_per_spell[[code]]$baseline <- c
          params$theatres$change_utilisation[[code]]$baseline <- 1 / (s * c)
        }) |>
          shiny::bindEvent(input[[spells_baseline_id]], input[[cases_baseline_id]])

        shiny::observe({
          s <- input[[spells_param_id]]
          c <- input[[cases_param_id]]

          params$theatres$spells_per_case[[code]]$interval <- s
          params$theatres$cases_per_spell[[code]]$interval <- c
          params$theatres$change_utilisation[[code]]$interval <- rev(1 / (s * c))
        }) |>
          shiny::bindEvent(input[[spells_param_id]], input[[cases_param_id]])
      })

    theatres_data <- shiny::reactiveValues(data = {
      # TODO: this should be loaded dynamically
      readRDS(app_sys("app", "data", "theatre_specialties.Rds")) |>
        dplyr::select(-"specialty") |>
        dplyr::mutate(
          dplyr::across("code", sanitize_input_name),
          # baseline_spells = c(1.6, 1.7, 2.2, 1.8, 5.4, 2.8, 2, 2.4, 2.6),
          # baseline_cases = c(1.4, 1, 1.1, 2.9, 1, 1.3, 1, 1.1, 1.2),
          baseline_spells = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
          baseline_cases = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
          spells_per_case = purrr::map(.data[["baseline_spells"]], `*`, c(0.95, 1.05)),
          cases_per_session = purrr::map(.data[["baseline_cases"]], `*`, c(0.95, 1.05))
        ) |>
        dplyr::group_nest(.data[["code"]]) |>
        tibble::deframe() |>
        purrr::map(as.list) |>
        purrr::map(purrr::flatten)
    })

    shiny::observe({
      td <- shiny::req(theatres_data)$data

      purrr::iwalk(
        td,
        \(.x, code) {
          shiny::updateNumericInput(
            session,
            glue::glue("spells_baseline_{code}"),
            value = .x$baseline_spells
          )
          shiny::updateSliderInput(
            session,
            glue::glue("spells_param_{code}"),
            value = .x$spells_per_case
          )
          shiny::updateNumericInput(
            session,
            glue::glue("cases_baseline_{code}"),
            value = .x$baseline_cases
          )
          shiny::updateSliderInput(
            session,
            glue::glue("cases_param_{code}"),
            value = .x$cases_per_session
          )
        }
      )
    }) |>
      shiny::bindEvent(theatres_data$data)

    shiny::observe({
      shiny::req(session$userData$data_loaded())
      p <- shiny::req(session$userData$params$theatres)

      theatres_data$data <- names(p$spells_per_case) |>
        purrr::set_names(sanitize_input_name) |>
        purrr::map(\(.x) {
          list(
            baseline_spells = p$spells_per_case[[.x]]$baseline,
            baseline_cases = p$cases_per_session[[.x]]$baseline,
            spells_per_case = p$spells_per_case[[.x]]$interval,
            cases_per_session = p$cases_per_session[[.x]]$interval
          )
        })
    }) |>
      shiny::bindEvent(session$userData$data_loaded())
  })
}
