#' health_status_adjustment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_health_status_adjustment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1("Health Status Adjustment"),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        md_file_to_html("app", "text", "health_status_adjustment.md"),
        shinyjs::hidden(
          shinyjs::disabled(
            shiny::checkboxInput(
              ns("enable_hsa"),
              "Enabled Health Status Adjustment?",
              TRUE
            )
          )
        )
      )
    )
  )
}
