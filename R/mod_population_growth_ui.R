#' popg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_population_growth_ui <- function(id) {
  ns <- shiny::NS(id)

  projections <- get_golem_config("population_projections")

  slider <- \(name, id, value = 0) shiny::sliderInput(
    ns(id),
    name,
    min = 0,
    max = 100,
    value = value
  )

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
        shinyjs::disabled(slider(projections[[1]], names(projections)[[1]], 100)),
        purrr::imap(projections[-1], slider)
      )
    )
  )
}
