#' mitigator_summary Server Functions
#'
#' @noRd
mod_mitigators_summary_server <- function(id, age_sex_data, params) {
  shiny::moduleServer(id, function(input, output, session) {
    mitigators_summary <- shiny::reactive({
      year <- as.character(shiny::req(params$start_year))

      strategy_codes <- app_sys("app", "data", "mitigator-codes.Rds") |>
        readr::read_rds() |>
        dplyr::select("strategy", "strategy_name", "mitigator_code")

      strategy_names <- get_golem_config("mitigators_config") |>
        purrr::map("strategy_subset") |>
        purrr::flatten() |>
        tibble::enframe("strategy", "strategy_name") |>
        tidyr::unnest("strategy_name") |>
        dplyr::left_join(
          strategy_codes,
          by = dplyr::join_by("strategy", "strategy_name")
        ) |>
        dplyr::mutate(
          strategy_name = glue::glue("{strategy_name} ({mitigator_code})")
        ) |>
        dplyr::select(-"mitigator_code")

      age_sex_data |>
        dplyr::filter(
          .data[["fyear"]] == year,
          .data[["provider"]] == params$dataset
        ) |>
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
