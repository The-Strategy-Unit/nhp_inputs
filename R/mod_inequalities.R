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
      column(6,
             bs4Dash::box(
               collapsible = FALSE,
               headerBorder = FALSE,
               width = 12,
               md_file_to_html("app", "text", "inequalities.md"),
               bs4Dash::box(
                 collapsible = FALSE,
                 headerBorder = FALSE,
                 width = 12,
                 md_file_to_html("app", "text", "inequalities_cont.md")
               )
             )
      ),
      column(6,
             bs4Dash::box(
               collapsible = FALSE,
               headerBorder = FALSE,
               width = 12,
               img(src='www/inequality.png', width = "100%")
             )
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 6,
        p("Please select the appropriate change below"),
        selectInput(ns("change"), "",
                    choices = c("No change", "Level up", "Level down",
                                "Zero sum redistribution")
        ))
    ),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        img(src='www/level_up.png', width = "100%")
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        img(src='www/level_down.png', width = "100%")
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        img(src='www/zero_sum.png', width = "100%")
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
