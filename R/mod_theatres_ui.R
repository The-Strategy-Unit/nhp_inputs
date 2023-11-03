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
