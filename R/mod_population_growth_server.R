#' popg Server Functions
#'
#' @noRd
mod_population_growth_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "demographic_factors")

  shiny::moduleServer(id, function(input, output, session) {
    projections <- get_population_growth_options(params$dataset)

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

    output$population_projection_text <- shiny::renderUI({
      pf <- population_factors() |>
        shiny::req() |>
        dplyr::summarise(
          .by = c("year"),
          dplyr::across("value", sum)
        ) |>
        tibble::deframe()

      baseline <- scales::comma(
        pf[["baseline"]],
        accuracy = 1,
        scale = 0.001,
        suffix = "k"
      )
      horizon <- scales::comma(
        pf[["horizon"]],
        accuracy = 1,
        scale = 0.001,
        suffix = "k"
      )

      rate <- scales::number(
        pf[["horizon"]] / pf[["baseline"]],
        accuracy = 0.001
      )

      htmltools::tags$div(
        htmltools::tags$p(
          glue::glue(
            "In the baseline year, the catchment population is {baseline} and in the horizon year it is {horizon}."
          )
        ),
        htmltools::tags$p(
          glue::glue(
            "This gives an overall activity growth rate of {rate}."
          )
        )
      )
    })

    output$pop_pyramid <- shiny::renderPlot({
      df <- population_factors()

      df |>
        dplyr::filter(.data$age < 90) |>
        dplyr::mutate(
          dplyr::across("value", \(.x) .x * ifelse(.data$sex == 1, -1, 1)),
          dplyr::across(
            "sex",
            \(.x) {
              forcats::fct_recode(
                factor(.x, levels = c("1", "2")),
                "Males" = "1",
                "Females" = "2"
              )
            }
          )
        ) |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data$age,
            y = .data$value,
            fill = .data$sex,
            group = paste0(.data$year, .data$sex)
          )
        ) +
        ggplot2::geom_col(
          data = \(.x) dplyr::filter(.x, .data$year == "baseline"),
          width = 1,
          position = "identity",
          alpha = 0.5,
          na.rm = TRUE
        ) +
        ggplot2::geom_step(
          ggplot2::aes(colour = .data$sex),
          data = \(.x) {
            .x <- dplyr::filter(.x, .data$year == "baseline")

            .x |>
              dplyr::bind_rows(
                .x |>
                  dplyr::filter(.data$age == max(.data$age)) |>
                  dplyr::mutate(age = .data$age + 1)
              )
          },
          position = ggplot2::position_nudge(x = -0.5),
          na.rm = TRUE
        ) +
        ggplot2::geom_step(
          data = \(.x) {
            .x <- dplyr::filter(.x, .data$year == "horizon")

            .x |>
              dplyr::bind_rows(
                .x |>
                  dplyr::filter(.data$age == max(.data$age)) |>
                  dplyr::mutate(age = .data$age + 1)
              )
          },
          position = ggplot2::position_nudge(x = -0.5),
          colour = "black",
          na.rm = TRUE
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          name = "Population Estimates",
          labels = \(.x) scales::comma(abs(.x))
        ) +
        ggplot2::scale_x_continuous(
          name = "Males",
          breaks = seq(0, 90, by = 10),
          sec.axis = ggplot2::sec_axis(~., name = "Females")
        ) +
        ggplot2::scale_colour_manual(
          values = c("Males" = "#5881c1", "Females" = "#ec6555")
        ) +
        ggplot2::scale_fill_manual(
          values = c("Males" = "#5881c1", "Females" = "#ec6555")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom"
        )
    })

    output$pop_rate <- shiny::renderPlot({
      df <- population_factors() |>
        tidyr::pivot_wider(
          names_from = "year",
          values_from = "value"
        )

      df |>
        dplyr::mutate(
          dplyr::across(
            "sex",
            \(.x) {
              forcats::fct_recode(
                factor(.x, levels = c("1", "2")),
                "Males" = "1",
                "Females" = "2"
              )
            }
          )
        ) |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data$age,
            y = .data$horizon / .data$baseline,
            colour = .data$sex
          )
        ) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(
          breaks = seq(0, 90, by = 10)
        ) +
        ggplot2::scale_colour_manual(
          values = c("Males" = "#5881c1", "Females" = "#ec6555")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom"
        ) +
        ggplot2::labs(x = "Age", y = "Growth Rate")
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
