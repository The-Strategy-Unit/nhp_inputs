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

#' reasons Server Functions
#'
#' @noRd
mod_reasons_server <- function(id, params, key) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      p <- session$userData$params
      shiny::updateTextAreaInput(session, "value", value = p$reasons[[key]])
    }) |>
      shiny::bindEvent(session$userData$data_loaded())

    shiny::observe({
      params$reasons[[key]] <- input$value
    }) |>
      shiny::bindEvent(input$value)
  })
}

## To be copied in the UI
#

## To be copied in the server
# mod_reasons_server("reasons_1")
