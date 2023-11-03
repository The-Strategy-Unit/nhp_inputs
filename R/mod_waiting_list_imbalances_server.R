#' wli Server Functions
#'
#' @noRd
mod_waiting_list_imbalances_server <- function(id, params) { # nolint: object_usage_linter.
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  mod_reasons_server(shiny::NS(id, "reasons"), params, "waiting_list_adjustment")

  shiny::moduleServer(id, function(input, output, session) {
    # static values ----
    table <- mod_waiting_list_imbalances_table() |>
      tidyr::pivot_longer(
        tidyselect::ends_with("id"),
        names_to = "activity_type",
        values_to = "id"
      ) |>
      dplyr::mutate(
        dplyr::across("activity_type", ~ stringr::str_sub(.x, 1, 2))
      )

    # reactives ----

    # load the waiting list data from azure
    baseline_data <- shiny::reactive({
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))

      df <- load_rds_from_adls(glue::glue("{dataset}/waiting_list_adjustment.rds"))[[year]]

      tretspef <- df[["tretspef"]]
      list("ip", "op") |>
        purrr::set_names() |>
        purrr::map(\(.x) purrr::set_names(as.list(df[[.x]]), tretspef))
    }) |>
      shiny::bindCache(params$dataset, params$start_year)

    # observers ----

    # update the time profile
    shiny::observe({
      params$time_profile_mappings[["waiting_list_adjustment"]] <- selected_time_profile()
    }) |>
      shiny::bindEvent(selected_time_profile())

    # when the baseline data is loaded, if any of the loaded values are 0 then the inputs should be disabled
    shiny::observe({
      bd <- shiny::req(baseline_data())

      purrr::pwalk(
        table,
        \(activity_type, id, code, ...) {
          v <- bd[[activity_type]][[code]] %||% 0
          if (v == 0) {
            shinyjs::disable(id)
          }
        }
      )
    }) |>
      shiny::bindEvent(baseline_data())

    # create observers for all of the inputs in the table
    # when any of the values change, update the params. if the value is 0 then remove it from the params
    purrr::pwalk(
      table,
      \(activity_type, id, code, ...) {
        shiny::observe({
          v <- shiny::req(input[[id]])
          bd <- shiny::req(baseline_data())[[activity_type]][[code]] %||% 0

          change <- v / bd

          change_string <- scales::percent(change, 0.01)

          shiny::updateTextInput(session, paste0(id, "_output"), value = change_string)

          params[["waiting_list_adjustment"]][[activity_type]][[code]] <- if (v != 0) v
        })
      }
    )

    # when the module is initialised, load the values from the loaded params file
    init <- shiny::observe({
      p <- params$waiting_list_adjustment

      # update the selected time profile
      update_time_profile(params$time_profile_mappings[["waiting_list_adjustment"]])

      table |>
        purrr::pwalk(\(activity_type, id, code, ...) {
          v <- p[[activity_type]][[code]] %||% 0

          shiny::updateNumericInput(session, id, value = v)
        })

      init$destroy()
    })
  })
}
