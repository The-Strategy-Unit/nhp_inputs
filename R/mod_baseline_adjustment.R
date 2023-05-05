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

  bs4Dash::box()
}

#' baseline_adjustment Server Functions
#'
#' @noRd
mod_baseline_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}
