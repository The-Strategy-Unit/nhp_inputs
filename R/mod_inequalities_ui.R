#' inequalities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inequalities_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1("Inequalities"),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        md_file_to_html("app", "text", "inequalities.md")
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 8,
        div(
          shiny::downloadButton(
            ns("download_inequalities"),
            "Download inequalities"
          ),
          actionButton(ns("set_all_zero_sum"), "Set all to zero sum"),
          actionButton(ns("clear_all"), "Clear all", class = "btn-secondary")
        ),
        DT::dataTableOutput(ns("hrg_table"), height = "calc(100vh - 200px)")
      )
    )
  )
}
