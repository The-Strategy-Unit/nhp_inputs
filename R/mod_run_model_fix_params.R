mod_run_model_fix_params <- function(p, schema_text) {
  p <- mod_run_model_remove_invalid_mitigators(p, schema_text)

  # nolint start: commented_code_linter
  # some of the items in our params will be lists of length 0.
  # jsonlite will serialize these as empty arrays
  #   toJSON(list()) == "[]"
  # we need to have these serialize as empty objects, so we need to replace
  # these with NULL's as
  #   toJSON(NULL) == "{}"
  # nolint end
  recursive_nullify <- function(.x) {
    for (i in names(.x)) {
      if (length(.x[[i]]) == 0) {
        .x[i] <- list(NULL)
      } else {
        .x[[i]] <- recursive_nullify(.x[[i]])
      }
    }
    .x
  }
  p <- recursive_nullify(p)

  # need to convert financial year to calendar year
  p$start_year <- as.integer(stringr::str_sub(p$start_year, 1, 4))

  # reorder the params
  p_order <- c(
    "user",
    "dataset",
    "scenario",
    "seed",
    "model_runs",
    "start_year",
    "end_year",
    "app_version",
    "interval_type",
    "demographic_factors",
    "health_status_adjustment",
    "baseline_adjustment",
    "waiting_list_adjustment",
    "expat",
    "repat_local",
    "repat_nonlocal",
    "inequalities",
    "non-demographic_adjustment",
    "activity_avoidance",
    "efficiencies",
    "reasons"
  )

  # make sure to only select items that exist in the params
  p_order <- p_order[p_order %in% names(p)]
  # add in any items in the params that aren't in the order at the end
  p_order <- c(p_order, setdiff(names(p), p_order))

  p[p_order]
}
