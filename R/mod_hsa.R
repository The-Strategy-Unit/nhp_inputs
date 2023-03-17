#' hsa UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hsa_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::h1("Slider"),
    shiny::sliderInput(ns("slider"),
      label = "slider",
      min = 0,
      max = 200,
      post = "%",
      value = c(40, 60)
    )
  )
}

#' hsa Server Functions
#'
#' @noRd
mod_hsa_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      params$health_status_adjustment <- shiny::req(input$slider) / 100
    })
  })
}
