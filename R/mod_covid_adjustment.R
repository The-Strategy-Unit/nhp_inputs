#' covid_adjustment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_covid_adjustment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1("Health Status Adjustment"),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        md_file_to_html("app", "text", "covid_adjustment.md")
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        gt::gt_output(ns("covid_adjustment_table"))
      )
    )
  )
}

#' covid_adjustment Server Functions
#'
#' @noRd
mod_covid_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    covid_adjustment <- shiny::reactive({
      if (params$start_year != "201920") {
        return(
          list(
            "aae" = list(
              "ambulance" = c(1.0, 1.0),
              "walk-in" = c(1.0, 1.0)
            ),
            "ip" = list(
              "elective" = c(1.0, 1.0),
              "maternity" = c(1.0, 1.0),
              "non-elective" = c(1.0, 1.0)
            ),
            "op" = list(
              "first" = c(1.0, 1.0),
              "followup" = c(1.0, 1.0),
              "procedure" = c(1.0, 1.0)
            )
          )
        )
      }

      ds <- shiny::req(params$dataset)

      glue::glue("{ds}/covid_adjustment.rds") |>
        load_rds_from_adls() |>
        purrr::map_depth(2, `*`, 1 + c(-1, 1) * 0.025 / 12) # 5% either side, but adjusted to be for 1 month
    }) |>
      shiny::bindCache(params$dataset, params$start_year)

    output$covid_adjustment_table <- gt::render_gt({
      shiny::req(params$start_year == "201920")

      covid_adjustment() |>
        purrr::map_depth(2, ~ purrr::set_names(.x, c("low", "high"))) |>
        purrr::map(tibble::enframe) |>
        dplyr::bind_rows(.id = "activity_type") |>
        dplyr::mutate(
          dplyr::across(
            "activity_type",
            ~ forcats::fct_recode(
              .x,
              "Accident and Emergency" = "aae",
              "Inpatients" = "ip",
              "Outpatients" = "op"
            )
          )
        ) |>
        tidyr::unnest_wider("value") |>
        gt::gt(rowname_col = "name", groupname_col = "activity_type") |>
        gt::tab_spanner(
          "90% Confidence Interval",
          columns = c("low", "high")
        ) |>
        gt::cols_label(
          "low" = "Low Estimate",
          "high" = "High Estimate"
        ) |>
        gt::tab_options(
          row_group.border.top.width = gt::px(2),
          row_group.border.top.color = "black",
          row_group.border.bottom.color = "black",
          row_group.background.color = "#686f73"
        )
    })

    shiny::observe({
      params$covid_adjustment <- covid_adjustment()
    })
  })
}
