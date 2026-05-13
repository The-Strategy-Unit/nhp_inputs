#' wli Server Functions
#'
#' @noRd
mod_waiting_list_imbalances_server <- function(id, wli_data, params) {
  mod_reasons_server(
    shiny::NS(id, "reasons"),
    params,
    "waiting_list_adjustment"
  )

  shiny::moduleServer(id, function(input, output, session) {
    # static values ----
    multipliers <- readr::read_csv(
      "inst/app/data/waiting_list_params.csv",
      col_types = "cddddd"
    ) |>
      dplyr::transmute(
        .data[["tretspef"]],
        ip = .data[["mixed_split"]] *
          .data[["avg_ip_activity_per_pathway_mixed"]],
        op = .data[["op_only_split"]] *
          .data[["avg_op_first_activity_per_pathway_op_only"]] +
          .data[["mixed_split"]] *
            .data[["avg_op_first_activity_per_pathway_mixed"]]
      ) |>
      tidyr::pivot_longer(
        c("ip", "op"),
        names_to = "activity_type",
        values_to = "multiplier"
      )

    # reactives ----

    # load the waiting list data from azure
    baseline_data <- shiny::reactive({
      # nolint start: object_usage_linter
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))
      # nolint end

      wli_data |>
        dplyr::filter(
          .data[["provider"]] == .env[["dataset"]],
          .data[["fyear"]] == .env[["year"]]
        ) |>
        dplyr::select(-"provider", -"fyear") |>
        tidyr::pivot_longer(
          c("ip", "op"),
          names_to = "activity_type",
          values_to = "count"
        ) |>
        dplyr::filter(.data[["count"]] > 0) |>
        dplyr::inner_join(
          multipliers,
          by = dplyr::join_by("tretspef", "activity_type")
        ) |>
        dplyr::transmute(
          .data[["tretspef"]],
          .data[["activity_type"]],
          .data[["count"]],
          param = 1 +
            .data[["avg_change"]] * .data[["multiplier"]] / .data[["count"]]
        )
    })

    # observers ----

    # when the module is initialised, load the values from the loaded params file
    init <- shiny::observe(
      {
        p <- shiny::isolate({
          params
        })

        p <- p$waiting_list_adjustment

        if (length(p$ip) + length(p$op) > 0) {
          shinyWidgets::updateSwitchInput(
            session,
            "use_wli",
            value = TRUE
          )
        }

        init$destroy()
      },
      priority = 10
    )

    shiny::observe({
      params[["waiting_list_adjustment"]] <- if (input$use_wli) {
        baseline_data() |>
          dplyr::select("activity_type", "tretspef", "param") |>
          dplyr::group_nest(.data[["activity_type"]]) |>
          dplyr::mutate(
            dplyr::across(
              "data",
              \(.x) purrr::map(.x, purrr::compose(as.list, tibble::deframe))
            )
          ) |>
          tibble::deframe()
      } else {
        list(ip = list(), op = list())
      }
    }) |>
      shiny::bindEvent(input$use_wli)

    # renders ----

    output$table <- gt::render_gt({
      baseline_data() |>
        dplyr::filter(.data$count > 5) |>
        mod_waiting_list_imbalances_table()
    })
  })
}
