#' run_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_run_model_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    col_4(
      mod_reasons_ui(ns("reasons")),
      bs4Dash::box(
        title = "Run Model",
        width = 12,
        shiny::fluidRow(
          col_6(
            shiny::actionButton(ns("submit"), "Submit Model Run"),
          ),
          col_6(
            shiny::downloadButton(ns("download_params"), "Download params")
          )
        ),
        shiny::uiOutput(ns("status"))
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 12,
        md_file_to_html("app", "text", "run_model.md")
      )
    ),
    bs4Dash::box(
      title = "View Params",
      width = 8,
      collapsed = TRUE,
      shiny::verbatimTextOutput(ns("params_json"))
    )
  )
}
