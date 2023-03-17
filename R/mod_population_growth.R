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

  purrr::imap(
    projections,
    \(.x, .i) shiny::sliderInput(ns(.i), .x, min = 0, max = 100, value = (.i == "principal_proj") * 100)
  )
}

#' popg Server Functions
#'
#' @noRd
mod_population_growth_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    projections <- get_golem_config("population_projections")
    changeable_projections <- names(projections)[-1]

    stopifnot(
      "Principal Projection should be the base case" = names(projections)[[1]] == "principal_proj"
    )

    # the principal projection is the base case, and is set to be the complement of the sum ofth e other sliders
    shinyjs::disable("principal_proj")

    purrr::map(
      changeable_projections,
      \(.x) {
        shiny::observe({
          new_values <- reduce_values(values()[changeable_projections], .x)

          purrr::iwalk(
            new_values[-which(names(new_values) == .x)],
            \(.x, .i) shiny::updateSliderInput(session, .i, value = .x * 100)
          )

          shiny::updateSliderInput(session, "principal_proj", value = (1 - sum(new_values)) * 100)
        }) |>
          shiny::bindEvent(input[[.x]])
      }
    )

    values <- shiny::reactive({
      purrr::map_dbl(
        shiny::reactiveValuesToList(input),
        `/`,
        100
      )[names(projections)]
    })

    shiny::observe({
      v <- values()

      params[["demographic_factors"]][["variant_probabilities"]] <- as.list(v[v > 0])
    })
  })
}
