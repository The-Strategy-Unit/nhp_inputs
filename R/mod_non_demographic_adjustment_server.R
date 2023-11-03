#' Non Demographic Server Functions
#'
#' @noRd
mod_non_demographic_adjustment_server <- function(id, params) { # nolint: object_usage_linter.
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  mod_reasons_server(shiny::NS(id, "reasons"), params, "non-demographic_adjustment")

  nda_values <- purrr::imap(
    nda_groups,
    \(.x, .i) {
      purrr::map(
        purrr::set_names(names(.x$values)),
        \(.y) mod_non_demographic_adjustment_input_server(id, .y, .i, params)
      )
    }
  )

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      params[["non-demographic_adjustment"]] <- nda_values |>
        purrr::map(\(.x) {
          .x |>
            purrr::map(rlang::exec) |>
            purrr::keep("include") |>
            purrr::map("values")
        })
    })

    # update the time profile
    shiny::observe({
      params$time_profile_mappings[["non-demographic_adjustment"]] <- selected_time_profile()
    }) |>
      shiny::bindEvent(selected_time_profile())
  })
}
