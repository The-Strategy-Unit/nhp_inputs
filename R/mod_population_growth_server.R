#' popg Server Functions
#'
#' @noRd
mod_population_growth_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "demographic_factors")

  shiny::moduleServer(id, function(input, output, session) {
    load_pop_factors <- function(file) {
      p <- shiny::req(params)
      v <- shiny::req(input$population_projection)
      sy <- stringr::str_sub(p$start_year, 0, 4)
      ey <- as.character(p$end_year)

      arrow::open_dataset(file.path("app_data", paste0(file, ".parquet"))) |>
        dplyr::filter(
          .data$fyear == as.integer(p$start_year),
          .data$provider == p$dataset,
          .data$variant == v
        ) |>
        dplyr::select(
          "age",
          "sex",
          dplyr::all_of(c("baseline" = sy, "horizon" = ey))
        ) |>
        dplyr::collect() |>
        tidyr::pivot_longer(
          c("baseline", "horizon"),
          names_to = "year",
          values_to = "value"
        ) |>
        dplyr::arrange(.data$year, .data$sex, .data$age)
    }

    factors_to_list <- function(df) {
      df |>
        dplyr::summarise(
          .by = c("year", "sex", "age"),
          dplyr::across("value", list)
        ) |>
        dplyr::group_nest(.data$year, .data$sex) |>
        dplyr::mutate(
          dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))
        ) |>
        dplyr::group_nest(.data$year) |>
        dplyr::mutate(
          dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))
        ) |>
        tibble::deframe()
    }

    population_factors <- shiny::reactive({
      load_pop_factors("demographic_factors")
    })

    birth_factors <- shiny::reactive({
      load_pop_factors("birth_factors")
    })

    # the initial state might not have values for the population/births section of
    # the demographic_adjustment params entry.
    # but, because the default variant might not change the selection in the ui, the observer
    # to update the params will not fire.
    # we use this reactiveVal to always trigger inserting the initial values into the params.
    init <- shiny::reactiveVal(NULL)
    shiny::observe({
      shiny::updateSelectInput(
        session,
        "population_projection",
        selected = params$demographic_adjustment$variant
      )
      init(TRUE)
    }) |>
      shiny::bindEvent(params, once = TRUE)

    shiny::observe({
      params$demographic_adjustment <- list(
        "variant" = input$population_projection,
        "population" = factors_to_list(population_factors()),
        "births" = factors_to_list(birth_factors())
      )
    }) |>
      shiny::bindEvent(input$population_projection, init())
  })
}
