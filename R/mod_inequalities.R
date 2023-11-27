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
        width = 6,
        md_file_to_html("app", "text", "inequalities.md")
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 6,
        p("GRAPH")
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 12,
        md_file_to_html("app", "text", "inequalities_cont.md")
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        img(src='www/level_up.png')
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        md_file_to_html("app", "text", "inequalities.md")
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        md_file_to_html("app", "text", "inequalities.md")
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
