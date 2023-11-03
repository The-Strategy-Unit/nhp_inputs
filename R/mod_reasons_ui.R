#' reasons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reasons_ui <- function(id) {
  ns <- shiny::NS(id)
  bs4Dash::box(
    title = "Reasons",
    width = 12,
    shiny::textAreaInput(
      ns("value"),
      NULL,
      height = "200px"
    )
  )
}
