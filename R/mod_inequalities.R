#' inequalities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inequalities_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::tags$h1("Inequalities"),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 12,
        md_file_to_html("app", "text", "covid_adjustment.md")
      )
    )
  )
}

#' inequalities Server Functions
#'
#' @noRd
mod_inequalities_server <- function(id, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
