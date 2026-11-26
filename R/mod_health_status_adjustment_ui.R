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
      col_4(
        bs4Dash::box(
          title = "About",
          collapsible = FALSE,
          width = 12,
          md_file_to_html("app", "text", "health_status_adjustment.md")
        )
      ),
      bs4Dash::box(
        title = "Health Status Adjustment",
        collapsible = FALSE,
        width = 8,
        shinyjs::hidden(
          shinyjs::disabled(
            shiny::checkboxInput(
              ns("enable_hsa"),
              "Enabled",
              TRUE
            )
          )
        )
      )
    )
  )
}
