#' mod_covid_adjustment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_covid_adjustment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList()
}

#' mod_covid_adjustment Server Functions
#'
#' @noRd
mod_mod_covid_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    covid_adjustment <- shiny::reactive({
      ds <- shiny::req(params$dataset)

      glue::glue("{ds}/covid_adjustment.rds") |>
        load_rds_from_adls() |>
        purrr::map_depth(2, `*`, c(0.975, 1.025))
    }) |>
      shiny::bindCache(params$dataset)

    shiny::observe({
      params$covid_adjustment <- covid_adjustment()
    })
  })
}
