#' Get population growth options
#'
#' Retrieves the available population growth projection options for a given dataset
#' from the golem configuration.
#'
#' @param dataset Dataset identifier used to retrieve dataset-specific configuration.
#'
#' @return A named list of population growth projection values.
#' @noRd
get_population_growth_options <- function(dataset) {
  projections <- get_golem_config(
    "population_projections",
    config = paste0("dataset_", dataset)
  )

  subset <- projections$subset
  if (is.null(subset)) {
    subset <- names(projections$values)
  }

  projections$values[subset]
}
