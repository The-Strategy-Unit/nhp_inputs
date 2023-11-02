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
  if (!shiny::is.reactive(key)) {
    # copy the value, otherwise we will enter an infinite loop
    v <- key
    key <- shiny::reactive(v)
  }

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      p <- session$userData$params

      shiny::updateTextAreaInput(session, "value", value = purrr::pluck(p$reasons, !!!key()))
    }) |>
      shiny::bindEvent(session$userData$data_loaded(), key())

    shiny::observe({
      purrr::pluck(params$reasons, !!!key()) <- input$value
    }) |>
      shiny::bindEvent(input$value)
  })
}
