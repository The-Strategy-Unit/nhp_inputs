#' mitigator_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mitigators_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::tags$h1("Summary Totals"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          title = "About",
          collapsible = FALSE,
          width = 12,
          md_file_to_html("app", "text", "summary_totals.md")
        )
      ),
      col_6(
        bs4Dash::box(
          title = "TPMA Activity",
          collapsible = FALSE,
          width = 12,
          shinycssloaders::withSpinner(
            shiny::htmlOutput(ns("diagnoses_table"))
          )
        )
      )
    )
  )
}
