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
        md_file_to_html("app", "text", "health_status_adjustment.md")
      )
    )
  )
}

#' health_status_adjustment Server Functions
#'
#' @noRd
mod_health_status_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    # nothing to implement
  })
}
