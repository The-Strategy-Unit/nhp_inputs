#' baseline_adjustment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_baseline_adjustment_ui <- function(id) {
  ns <- shiny::NS(id)

  specs <- rtt_specialties() |>
    dplyr::mutate(sanitized_code = sanitize_input_name(.data[["code"]]))

  create_table <- function(at, g, df = specs) {
    df |>
      dplyr::mutate(
        param = purrr::map(
          .data[["sanitized_code"]],
          \(.x) shinyjs::disabled(
            shiny::sliderInput(
              ns(glue::glue("param_{at}_{g}_{.x}")),
              label = NULL,
              min = 0,
              max = 2,
              value = c(0.95, 1.05),
              step = 0.001
            )
          ) |>
            as.character() |>
            gt::html()
        ),
        include = purrr::map(
          .data[["sanitized_code"]],
          ~ shiny::checkboxInput(
            ns(glue::glue("include_{at}_{g}_{.x}")),
            label = NULL
          ) |>
            as.character() |>
            gt::html()
        )
      ) |>
      dplyr::select(-tidyselect::ends_with("code")) |>
      gt::gt(rowname_col = "specialty") |>
      gt::cols_label(
        param ~ "Confidence Interval",
        include ~ "Include?"
      ) |>
      gt::tab_options(table.width = gt::pct(100)) |>
      gt::as_raw_html()
  }

  shiny::tagList(
    shiny::tags$h1("Baseline Adjustment"),
    bs4Dash::tabsetPanel(
      shiny::tabPanel(
        "Inpatients",
        bs4Dash::tabsetPanel(
          shiny::tabPanel(
            "Elective",
            create_table("ip", "elective")
          ),
          shiny::tabPanel(
            "Non-Elective",
            create_table("ip", "non-elective")
          ),
          shiny::tabPanel(
            "Maternity",
            create_table("ip", "maternity", specs |> dplyr::filter(.data[["code"]] == "Other (Medical)"))
          )
        )
      ),
      shiny::tabPanel(
        "Outpatients",
        bs4Dash::tabsetPanel(
          shiny::tabPanel(
            "First Attendance",
            create_table("op", "first")
          ),
          shiny::tabPanel(
            "Follow-up Attendance",
            create_table("op", "followup")
          ),
          shiny::tabPanel(
            "Procedure",
            create_table("op", "procedure")
          )
        )
      ),
      shiny::tabPanel(
        "A&E",
        create_table(
          "aae",
          "-",
          tibble::tibble(code = c("ambulance", "walk-in")) |>
            dplyr::mutate(
              dplyr::across(
                "code",
                .fns = c(
                  specialty = snakecase::to_title_case,
                  sanitized_code = sanitize_input_name
                ),
                .names = "{.fn}"
              )
            )
        )
      )
    )
  )
}

#' baseline_adjustment Server Functions
#'
#' @noRd
mod_baseline_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    specs <- rtt_specialties() |>
      dplyr::mutate(sanitized_code = sanitize_input_name(.data[["code"]])) |>
      dplyr::cross_join(
        dplyr::bind_rows(
          ip = tibble::tibble(g = c("elective", "non-elective", "maternity")),
          op = tibble::tibble(g = c("first", "followup", "procedure")),
          .id = "at"
        )
      ) |>
      dplyr::filter(.data[["g"]] != "maternity" | .data[["code"]] == "Other (Medical)") |>
      dplyr::bind_rows(
        tibble::tibble(
          at = "aae",
          g = "-",
          code = c("ambulance", "walk-in")
        ) |>
          dplyr::mutate(
            dplyr::across(
              "code",
              .fns = c(
                specialty = snakecase::to_title_case,
                sanitized_code = sanitize_input_name
              ),
              .names = "{.fn}"
            )
          )
      ) |>
      dplyr::mutate(id = glue::glue("{at}_{g}_{sanitized_code}"))

    shiny::observe({
      # initialise the baseline adjustment
      params[["baseline_adjustment"]] <- list(
        ip = list(),
        op = list(),
        aae = list()
      )
    })

    specs |>
      purrr::pwalk(\(code, at, g, id, ...) {
        include_id <- glue::glue("include_{id}")
        param_id <- glue::glue("param_{id}")

        shiny::observe({
          i <- input[[include_id]]
          shinyjs::toggleState(param_id, i)

          if (at == "aae") {
            params[["baseline_adjustment"]][[at]][[code]] <- if (i) input[[param_id]]
          } else {
            params[["baseline_adjustment"]][[at]][[g]][[code]] <- if (i) input[[param_id]]
          }
        }) |>
          shiny::bindEvent(input[[include_id]], input[[param_id]])
      })
  })
}
