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

  specs <- get_lookups()[["rtt_specialties"]]

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
    bslib::layout_columns(
      col_widths = c(4, 8),
      fill = FALSE,
      shiny::tagList(
        bslib::card(
          fill = FALSE,
          md_file_to_html("app", "text", "baseline_adjustment.md"),
          shinyjs::hidden(
            shiny::downloadButton(
              ns("download_baseline"),
              "Download Baseline Values (excel)",
              class = "bg-secondary"
            )
          )
        ),
        mod_reasons_ui(ns("reasons"))
      ),
      bslib::card(
        bslib::card_header(
          "Parameters",
          class = "bg-primary"
        ),
        fill = FALSE,
        bslib::navset_tab(
          bslib::nav_panel(
            "Inpatients",
            bslib::navset_tab(
              bslib::nav_panel(
                "Elective",
                create_table("ip", "elective")
              ),
              bslib::nav_panel(
                "Non-Elective",
                create_table("ip", "non-elective")
              ),
              bslib::nav_panel(
                "Maternity",
                create_table(
                  "ip",
                  "maternity",
                  specs |> dplyr::filter(.data[["code"]] == "Other (Medical)")
                )
              )
            )
          ),
          bslib::nav_panel(
            "Outpatients",
            bslib::navset_tab(
              bslib::nav_panel(
                "First Attendance",
                create_table("op", "first")
              ),
              bslib::nav_panel(
                "Follow-up Attendance",
                create_table("op", "followup")
              ),
              bslib::nav_panel(
                "Procedure",
                create_table("op", "procedure")
              )
            )
          ),
          bslib::nav_panel(
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
