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
