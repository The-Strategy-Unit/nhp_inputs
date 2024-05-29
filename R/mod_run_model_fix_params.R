mod_run_model_fix_params <- function(p) {
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
      "covid_adjustment",
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

  # some of the items in our params will be lists of length 0.
  # jsonlite will serialize these as empty arrays
  #   toJSON(list()) == "[]"
  # we need to have these serialize as empty objects, so we need to replace
  # these with NULL's as
  #   toJSON(NULL) == "{}"
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

  # generate an id based on the dataset, scenario, and a hash of the params
  # make sure to add the user after creating the hash
  # the id must be at most 63 characters long, and must match the regex:
  #   '[a-z0-9]([-a-z0-9]*[a-z0-9])?'
  scenario_sanitized <- p$scenario |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "-")
  hash <- digest::digest(p, "crc32", serialize = TRUE)

  p$id <- glue::glue("{p$dataset}-{scenario_sanitized}") |>
    stringr::str_sub(1, 62 - stringr::str_length(hash)) |>
    stringr::str_to_lower() |>
    paste0("-", hash)

  # reorder the params
  p_order <- c(
    "id",
    "user",
    "dataset",
    "scenario",
    "seed",
    "model_runs",
    "start_year",
    "end_year",
    "app_version",
    "create_datetime",
    "interval_type",
    "demographic_factors",
    "baseline_adjustment",
    "covid_adjustment",
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
