#' covid_adjustment Server Functions
#'
#' @noRd
mod_covid_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    # reactives ----

    # load the covid adjustment values if the year is 201920, otherwise just use values of 1 to indicate no change
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
        purrr::map_depth(2, `*`, 1 + c(-1, 1) * 0.025 / 12) |>  # 5% either side, but adjusted to be for 1 month
        purrr::map_depth(2, janitor::round_half_up, 4)  # 4dp used for model, so present 4dp too
    }) |>
      shiny::bindCache(params$dataset, params$start_year)

    # observers ----

    # the covid adjustment values are fixed, load them straight into the parameters
    shiny::observe({
      params$covid_adjustment <- covid_adjustment()
    })

    # renders ----

    # shows the selected values for the covid adjustment if the year is 2019/20
    output$covid_adjustment_table <- gt::render_gt({
      shiny::req(params$start_year == "201920")

      mod_covid_adjustment_table(covid_adjustment())
    })
  })
}
