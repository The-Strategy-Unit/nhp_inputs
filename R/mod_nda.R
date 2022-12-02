#' nda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nda_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::selectInput(ns('slider'),
                       label = 'Activity Type',
                       choices = c('Non-Elective',
                                   'Elective',
                                   'Maternity'))

  )
}

#' nda Server Functions
#'
#' @noRd
mod_nda_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_nda_ui("nda_1")

## To be copied in the server
# mod_nda_server("nda_1")
