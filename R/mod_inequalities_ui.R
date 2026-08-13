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
    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::tagList(
        bslib::card(
          fill = FALSE,
          md_file_to_html("app", "text", "inequalities.md")
        ),
        mod_reasons_ui(ns("reasons"))
      ),
      bslib::card(
        fill = TRUE,
        shiny::div(
          shiny::downloadButton(
            ns("download_inequalities"),
            "Download inequalities"
          ),
          shiny::actionButton(ns("set_all_zero_sum"), "Set all to zero sum"),
          shiny::actionButton(
            ns("clear_all"),
            "Clear all",
            class = "btn-secondary"
          )
        ),
        shiny::br(),
        shiny::div(
          DT::dataTableOutput(ns("hrg_table"), height = "calc(100vh - 200px)")
        )
      )
    )
  )
}
