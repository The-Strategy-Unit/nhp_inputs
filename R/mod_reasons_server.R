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
      shiny::updateTextAreaInput(session, "value", value = purrr::pluck(params$reasons, !!!k()) %||% "")
    }) |>
      shiny::bindEvent(k())

    shiny::observe({
      purrr::pluck(params$reasons, !!!k()) <- input$value
    }) |>
      shiny::bindEvent(input$value)
  })
}
