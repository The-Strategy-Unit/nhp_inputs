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
mod_reasons_server <- function(id, params, ..., key = NULL) {
  ix <- c(...)

  shiny::moduleServer(id, function(input, output, session) {
    k <- shiny::reactive({
      if (is.null(key)) {
        return(ix)
      }

      c(ix, shiny::req(key()))
    })

    shiny::observe({
      purrr::pluck(params$reasons, !!!ix) <- purrr::pluck(session$userData$params$reasons, !!!ix)
    }) |>
      shiny::bindEvent(session$userData$data_loaded())

    shiny::observe({
      shiny::updateTextAreaInput(session, "value", value = purrr::pluck(params$reasons, !!!k()) %||% "")
    }) |>
      shiny::bindEvent(session$userData$data_loaded(), k())

    shiny::observe({
      purrr::pluck(params$reasons, !!!k()) <- input$value
    }) |>
      shiny::bindEvent(input$value)
  })
}
