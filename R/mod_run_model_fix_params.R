mod_run_model_fix_params <- function(p, schema_text) {
  # the time profiles may be empty, ensure that's not the case
  tpm <- p[["time_profile_mappings"]]
  p[["time_profile_mappings"]][["activity_avoidance"]] <- list(
    ip = tpm[["activity_avoidance"]][["ip"]] %||% list(),
    op = tpm[["activity_avoidance"]][["op"]] %||% list(),
    aae = tpm[["activity_avoidance"]][["aae"]] %||% list()
  )
  p[["time_profile_mappings"]][["efficiencies"]] <- list(
    ip = tpm[["efficiencies"]][["ip"]] %||% list(),
    op = tpm[["efficiencies"]][["op"]] %||% list()
  )

  # for a while, the wrong time profile was set for some items. force these to none
  p[["time_profile_mappings"]][
    c(
      "baseline_adjustment",
      "non-demographic_adjustment"
    )
  ] <- "none"

  # check all the mitigators have a time profile
  for (i in c("activity_avoidance", "efficiencies")) {
    for (j in names(p[[i]])) {
      for (k in names(p[[i]][[j]])) {
        if (is.null(p$time_profile_mappings[[i]][[j]][[k]])) {
          p$time_profile_mappings[[i]][[j]][[k]] <- "linear"
        }
      }
    }
  }

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
    "time_profile_mappings",
    "reasons"
  )

  # make sure to only select items that exist in the params
  p_order <- p_order[p_order %in% names(p)]
  # add in any items in the params that aren't in the order at the end
  p_order <- c(p_order, setdiff(names(p), p_order))

  p[p_order]
}
