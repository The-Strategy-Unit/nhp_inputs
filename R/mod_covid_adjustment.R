#' covid_adjustment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_covid_adjustment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList()
}

#' covid_adjustment Server Functions
#'
#' @noRd
mod_covid_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    covid_adjustment <- shiny::reactive({
      if (params$start_year != "201920") {
        return(
          list(
            "aae" = list(
              "ambulance" = c(1.0, 1.0),
              "walk-in" = c(1.0, 1.0)
            ),
            "ip" = list(
              "elective" = c(1.0, 1.0),
              "maternity" = c(1.0, 1.0),
              "non-elective" = c(1.0, 1.0)
            ),
            "op" = list(
              "first" = c(1.0, 1.0),
              "followup" = c(1.0, 1.0),
              "procedure" = c(1.0, 1.0)
            )
          )
        )
      }

      ds <- shiny::req(params$dataset)


      glue::glue("{ds}/covid_adjustment.rds") |>
        load_rds_from_adls() |>
        purrr::map_depth(2, `*`, 1 + c(-1, 1) * 0.025 / 12) # 5% either side, but adjusted to be for 1 month
    }) |>
      shiny::bindCache(params$dataset, params$start_year)

    shiny::observe({
      params$covid_adjustment <- covid_adjustment()
    })
  })
}
