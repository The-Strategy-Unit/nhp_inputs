#' mitigator_summary Server Functions
#'
#' @noRd
mod_mitigators_summary_server <- function(id, age_sex_data, params) {
  shiny::moduleServer(id, function(input, output, session) {
    mitigators_summary <- shiny::reactive({
      strategy_codes <- dplyr::select(
        get_lookups()[["mitigators"]],
        "strategy_name" = "strategy_name_full",
        "strategy"
      )

      age_sex_data() |>
        dplyr::count(
          .data[["strategy"]],
          wt = .data[["n"]],
          name = "total",
          sort = TRUE
        ) |>
        dplyr::slice(1:20) |>
        dplyr::inner_join(strategy_codes, by = dplyr::join_by("strategy")) |>
        dplyr::select(-"strategy")
    })

    output$diagnoses_table <- gt::render_gt({
      mitigators_summary() |>
        gt::gt(rowname_col = "strategy_name") |>
        gt::cols_label(
          "total" = "Total"
        ) |>
        gt::fmt_number(
          c("total"),
          decimals = 0,
          use_seps = TRUE
        )
    })
  })
}
