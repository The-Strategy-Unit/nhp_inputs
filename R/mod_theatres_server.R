#' theatres Server Functions
#'
#' @noRd
mod_theatres_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "theatres")
  shiny::moduleServer(id, function(input, output, session) {
    mod_theatres_load_specialties() |>
      purrr::pwalk(\(code, sanitized_code, ...) {
        spells_baseline_id <- glue::glue("spells_baseline_{sanitized_code}")
        spells_param_id <- glue::glue("spells_param_{sanitized_code}")
        cases_baseline_id <- glue::glue("cases_baseline_{sanitized_code}")
        cases_param_id <- glue::glue("cases_param_{sanitized_code}")

        shiny::observe({
          s <- input[[spells_baseline_id]]
          c <- input[[cases_baseline_id]]

          params$theatres$spells_per_case[[code]]$baseline <- s
          params$theatres$cases_per_spell[[code]]$baseline <- c
          params$theatres$change_utilisation[[code]]$baseline <- 1 / (s * c)
        }) |>
          shiny::bindEvent(input[[spells_baseline_id]], input[[cases_baseline_id]])

        shiny::observe({
          s <- input[[spells_param_id]]
          c <- input[[cases_param_id]]

          params$theatres$spells_per_case[[code]]$interval <- s
          params$theatres$cases_per_session[[code]]$interval <- c
          params$theatres$change_utilisation[[code]]$interval <- rev(1 / (s * c))
        }) |>
          shiny::bindEvent(input[[spells_param_id]], input[[cases_param_id]])
      })

    init <- shiny::observe(
      {
        p <- shiny::isolate({
          params$theatres
        })

        p$spells_per_case |>
          names() |>
          purrr::set_names(sanitize_input_name) |>
          purrr::iwalk(\(.x, .i) {
            shiny::updateNumericInput(
              session,
              glue::glue("spells_baseline_{.i}"),
              value = p$spells_per_case[[.x]]$baseline
            )
            shiny::updateSliderInput(
              session,
              glue::glue("spells_param_{.i}"),
              value = p$spells_per_case[[.x]]$interval
            )
            shiny::updateNumericInput(
              session,
              glue::glue("cases_baseline_{.i}"),
              value = p$cases_per_session[[.x]]$baseline
            )
            shiny::updateSliderInput(
              session,
              glue::glue("cases_param_{.i}"),
              value = p$cases_per_session[[.x]]$interval
            )
          })

        init$destroy()
      },
      priority = 20
    )
  })
}
