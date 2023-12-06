#' mitigator_summary Server Functions
#'
#' @noRd
mod_mitigators_summary_server <- function(id, provider_data, params) {
  shiny::moduleServer(id, function(input, output, session) {
    mitigators_summary <- shiny::reactive({
      year <- as.character(shiny::req(params$start_year))

      strategy_names <- get_golem_config("mitigators_config") |>
        purrr::map("strategy_subset") |>
        purrr::flatten() |>
        tibble::enframe("strategy", "strategy_name") |>
        tidyr::unnest("strategy_name")

      provider_data() |>
        purrr::map("age_sex") |>
        dplyr::bind_rows(.id = "strategy") |>
        dplyr::filter(.data[["fyear"]] == year) |>
        dplyr::count(.data[["strategy"]], wt = .data[["n"]], name = "total", sort = TRUE) |>
        dplyr::slice(1:20) |>
        dplyr::inner_join(strategy_names, by = dplyr::join_by("strategy")) |>
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
