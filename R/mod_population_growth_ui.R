#' popg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_population_growth_ui <- function(id, dataset) {
  ns <- shiny::NS(id)

  projections <- get_population_growth_options(dataset)
  default_projection <- names(projections)[[1]]

  shiny::tagList(
    shiny::tags$h1("Population Growth"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "population_growth.md")
        ),
        mod_reasons_ui(ns("reasons"))
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 8,
        shiny::selectInput(
          ns("population_projection"),
          label = "Projection",
          choices = stats::setNames(names(projections), projections),
          selected = default_projection
        )
      )
    )
  )
}
