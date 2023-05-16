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
    shinyjs::disabled(slider(projections[[1]], names(projections)[[1]], 100)),
    purrr::imap(projections[-1], slider)
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

    shiny::observe({
      shiny::req(session$userData$data_loaded())
      vp <- shiny::req(session$userData$params$demographic_factors$variant_probabilities)

      vp |>
        purrr::imap(\(.x, .i) {
          shiny::updateSliderInput(session, .i, value = .x * 100)
        })
    }) |>
      shiny::bindEvent(session$userData$data_loaded())

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
