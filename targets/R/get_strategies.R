get_strategies <- function(strategies_last_updated) {
  force(strategies_last_updated)

  con <- get_con("HESData")

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling_reference", "strategy_lookups")) |>
    dplyr::filter(!is.na(.data$strategy_type)) |>
    dplyr::arrange(.data$strategy_type, .data$strategy) |>
    dplyr::collect() |>
    dplyr::group_by(.data$strategy_type) |>
    dplyr::summarise(dplyr::across(.data$strategy, list)) |>
    tibble::deframe()
}
