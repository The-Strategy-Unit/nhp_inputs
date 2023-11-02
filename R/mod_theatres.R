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

  shiny::fluidRow(
    col_4(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 12,
        md_file_to_html("app", "text", "theatres.md")
      ),
      mod_reasons_ui(ns("reasons"))
    ),
    bs4Dash::box(
      title = "Theatres Utilisation",
      width = 8,
      gt::as_raw_html(mod_theatres_ui_table(ns), FALSE)
    )
  )
}

#' theatres Server Functions
#'
#' @noRd
mod_theatres_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "theatres")
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
          params$theatres$cases_per_session[[code]]$interval <- c
          params$theatres$change_utilisation[[code]]$interval <- rev(1 / (s * c))
        }) |>
          shiny::bindEvent(input[[spells_param_id]], input[[cases_param_id]])
      })

    init <- shiny::observe(
      {
        p <- shiny::isolate({
          params$theatres
        })

        p$spells_per_case |>
          names() |>
          purrr::set_names(sanitize_input_name) |>
          purrr::iwalk(\(.x, .i) {
            shiny::updateNumericInput(
              session,
              glue::glue("spells_baseline_{.i}"),
              value = p$spells_per_case[[.x]]$baseline
            )
            shiny::updateSliderInput(
              session,
              glue::glue("spells_param_{.i}"),
              value = p$spells_per_case[[.x]]$interval
            )
            shiny::updateNumericInput(
              session,
              glue::glue("cases_baseline_{.i}"),
              value = p$cases_per_session[[.x]]$baseline
            )
            shiny::updateSliderInput(
              session,
              glue::glue("cases_param_{.i}"),
              value = p$cases_per_session[[.x]]$interval
            )
          })

        init$destroy()
      },
      priority = 20
    )
  })
}
