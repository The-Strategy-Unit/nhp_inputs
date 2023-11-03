#' popg Server Functions
#'
#' @noRd
mod_population_growth_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "demographic_factors")

  shiny::moduleServer(id, function(input, output, session) {
    projections <- get_golem_config("population_projections")
    changeable_projections <- names(projections)[-1]

    stopifnot(
      "Principal Projection should be the base case" = names(projections)[[1]] == "principal_proj"
    )

    # when the module loads, run this observer once, and only once
    init <- shiny::observe(
      {
        # do not observe this event
        shiny::isolate({
          params$demographic_factors$variant_probabilities
        }) |>
          purrr::imap(\(.x, .i) {
            shiny::updateSliderInput(session, .i, value = .x * 100)
          })

        init$destroy()
      },
      # for some reason, not setting the priority to be low causes the values not to load
      priority = -1
    )

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
      input |>
        shiny::reactiveValuesToList() |>
        _[names(projections)] |>
        purrr::map_dbl(`/`, 100)
    })

    # without this timeout, the first time the observer runs it will overwrite whatever the init observer sets
    init_timeout <- TRUE
    shiny::observe({
      if (init_timeout) {
        shiny::invalidateLater(50)
        shiny::req((init_timeout <<- FALSE))
      }
      v <- values()

      params[["demographic_factors"]][["variant_probabilities"]] <- as.list(v[v > 0])
    }) |>
      shiny::bindEvent(values())
  })
}
