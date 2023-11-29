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
    # reactives ----

    avg_change <- shiny::reactive({
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))

      multipliers <- readr::read_csv("inst/app/data/waiting_list_params.csv", col_types = "cddddd") |>
        dplyr::transmute(
          .data[["tretspef"]],
          ip = .data[["mixed_split"]] * .data[["avg_ip_activity_per_pathway_mixed"]],
          op = .data[["op_only_split"]] * .data[["avg_op_first_activity_per_pathway_op_only"]] +
            .data[["mixed_split"]] * .data[["avg_op_first_activity_per_pathway_mixed"]]
        )

      # TODO: this needs to acutally be implemented
      multipliers |>
        dplyr::transmute(
          .data[["tretspef"]],
          change = 1
        ) |>
        dplyr::inner_join(multipliers, by = dplyr::join_by("tretspef")) |>
        dplyr::mutate(
          dplyr::across(c("ip", "op"), \(.x) .x * .data[["change"]])
        ) |>
        dplyr::select(-"change") |>
        tidyr::pivot_longer(-"tretspef", names_to = "activity_type", values_to = "change")
    }) |>
      shiny::bindCache(params$dataset, params$start_year)

    # load the waiting list data from azure
    baseline_data <- shiny::reactive({
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))

      glue::glue("{dataset}/waiting_list_adjustment.rds") |>
        load_rds_from_adls() |>
        purrr::pluck(year) |>
        tidyr::pivot_longer(-"tretspef", names_to = "activity_type", values_to = "count") |>
        dplyr::filter(.data[["count"]] > 0)
    }) |>
      shiny::bindCache(params$dataset, params$start_year)

    # the parameters to use in the model
    wli_params <- shiny::reactive({
      baseline_data() |>
        dplyr::inner_join(
          avg_change(),
          by = dplyr::join_by("tretspef", "activity_type")
        ) |>
        dplyr::arrange(.data[["tretspef"]]) |>
        dplyr::mutate(
          value = floor((1 - .data[["change"]]) * .data[["count"]])
        )
    })

    # observers ----

    # update the time profile
    shiny::observe({
      params$time_profile_mappings[["waiting_list_adjustment"]] <- selected_time_profile()
    }) |>
      shiny::bindEvent(selected_time_profile())

    # when the module is initialised, load the values from the loaded params file
    init <- shiny::observe({
      p <- params$waiting_list_adjustment

      # update the selected time profile
      update_time_profile(params$time_profile_mappings[["waiting_list_adjustment"]])

      if (length(p$ip) + length(p$op) > 0) {
        shinyWidgets::updateSwitchInput(
          session,
          "use_wli",
          value = TRUE
        )
      }

      init$destroy()
    })

    shiny::observe({
      params[["waiting_list_adjustment"]] <- if (input$use_wli) {
        wli_params() |>
          dplyr::select("activity_type", "tretspef", "value") |>
          dplyr::group_nest(.data[["activity_type"]]) |>
          dplyr::mutate(
            dplyr::across(
              "data",
              \(.x) purrr::map(.x, purrr::compose(as.list, tibble::deframe)))
          ) |>
          tibble::deframe()
      } else {
        list(ip = list(), op = list())
      }
    }) |>
      shiny::bindEvent(input$use_wli)

    # renders ----

    output$table <- gt::render_gt({
      mod_waiting_list_imbalances_table(x())
    })
  })
}
