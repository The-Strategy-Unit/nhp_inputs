#' popg Server Functions
#'
#' @noRd
mod_population_growth_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "demographic_factors")

  shiny::moduleServer(id, function(input, output, session) {
    projections <- get_population_growth_options(params$dataset)
    default_projection <- names(projections)[[1]]

    # when the module loads, run this observer once, and only once
    init <- shiny::observe(
      {
        if (is.null(params$demographic_factors$variant_probabilities)) {
          params$demographic_factors$variant_probabilities[[
            default_projection
          ]] <- 1
        }
        # do not observe this event
        shiny::isolate({
          params$demographic_factors$variant_probabilities
        }) |>
          names() |> # Assumes strictly 1 variant
          shiny::updateSelectInput(
            session,
            "population_projection",
            selected = _
          )

        init$destroy()
      },
      # for some reason, not setting the priority to be low causes the values not to load
      priority = -1
    )

    # without this timeout, the first time the observer runs it will overwrite whatever the init observer sets
    init_timeout <- TRUE
    shiny::observe({
      if (init_timeout) {
        shiny::invalidateLater(50)
        shiny::req((init_timeout <<- FALSE))
      }
      names(
        params$demographic_factors$variant_probabilities
      ) <- input$population_projection
    }) |>
      shiny::bindEvent(input$population_projection)
  })
}
