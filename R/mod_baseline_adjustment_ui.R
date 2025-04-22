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
        baseline = purrr::map(
          .data[["sanitized_code"]],
          \(.x) {
            shiny::textOutput(
              ns(glue::glue("baseline_{at}_{g}_{.x}"))
            ) |>
              as.character() |>
              gt::html()
          }
        ),
        adjustment = purrr::map(
          .data[["sanitized_code"]],
          \(.x) {
            shiny::sliderInput(
              ns(glue::glue("adjustment_{at}_{g}_{.x}")),
              label = NULL,
              min = -1,
              max = 1,
              value = 0,
              step = 1
            ) |>
              as.character() |>
              gt::html()
          }
        ),
        param = purrr::map(
          .data[["sanitized_code"]],
          \(.x) {
            shiny::textOutput(
              ns(glue::glue("param_{at}_{g}_{.x}"))
            ) |>
              as.character() |>
              gt::html()
          }
        )
      ) |>
      dplyr::select(-tidyselect::ends_with("code")) |>
      gt::gt(rowname_col = "specialty") |>
      gt::cols_label(
        baseline ~ "Baseline Count",
        adjustment ~ "Adjustment",
        param ~ "Relative Change"
      ) |>
      gt::tab_options(table.width = gt::pct(100)) |>
      gt::as_raw_html()
  }

  shiny::tagList(
    shiny::tags$h1("Baseline Adjustment"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "baseline_adjustment.md"),
          shinyjs::hidden(
            shiny::downloadButton(
              ns("download_baseline"),
              "Download Baseline Values (excel)"
            )
          )
        ),
        mod_reasons_ui(ns("reasons"))
      ),
      bs4Dash::box(
        title = "Parameters",
        width = 8,
        collapsible = FALSE,
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
                create_table(
                  "ip",
                  "maternity",
                  specs |> dplyr::filter(.data[["code"]] == "Other (Medical)")
                )
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
    )
  )
}
