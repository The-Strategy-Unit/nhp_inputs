get_catchments <- function(catchment_activity, pop_year_long) {
  catchment_activity |>
    dplyr::inner_join(
      pop_year_long,
      by = c("fyear", "sex", "age_grp", "lsoa11")
    ) |>
    dplyr::mutate(pop_catch = .data$pop * .data$p) |>
    dplyr::count(
      .data$fyear,
      .data$sex,
      .data$age_grp,
      provider = .data$procode3,
      wt = pop_catch,
      name = "pop_catch"
    ) |>
    dplyr::arrange(.data$fyear, .data$provider, .data$sex, .data$age_grp)
}
