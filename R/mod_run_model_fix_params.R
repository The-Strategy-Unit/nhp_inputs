mod_run_model_fix_params <- function(p, user) {
  # combine efficiences/activity avoidance items
  p[["efficiencies"]] <- list(
    ip = c(
      p[["efficiencies|mean_los"]]$ip,
      p[["efficiencies|aec"]]$ip,
      p[["efficiencies|preop"]]$ip,
      p[["efficiencies|bads"]]$ip
    ),
    op = c(
      p[["efficiencies|ctt"]]$op
    )
  )

  p[["activity_avoidance"]]$op <- c(
    p[["activity_avoidance|c2c"]]$op,
    p[["activity_avoidance|f2f"]]$op
  )

  p[["activity_avoidance"]]$aae <- c(
    p[["activity_avoidance|fa"]]$aae,
    p[["activity_avoidance|lbs"]]$aae,
    p[["activity_avoidance|lcd"]]$aae
  )

  # remove the items
  p[["efficiencies|mean_los"]] <- NULL
  p[["efficiencies|aec"]] <- NULL
  p[["efficiencies|preop"]] <- NULL
  p[["efficiencies|bads"]] <- NULL
  p[["efficiencies|ctt"]] <- NULL
  p[["activity_avoidance|c2c"]] <- NULL
  p[["activity_avoidance|f2f"]] <- NULL
  p[["activity_avoidance|fa"]] <- NULL
  p[["activity_avoidance|lbs"]] <- NULL
  p[["activity_avoidance|lcd"]] <- NULL

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
    stringr::str_sub(1, 63 - stringr::str_length(hash)) |>
    stringr::str_to_lower() |>
    paste0("-", hash)

  p$user <- user
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
    "demographic_factors",
    "covid_adjustment",
    "expat",
    "repat_local",
    "repat_nonlocal",
    "baseline_adjustment",
    "waiting_list_adjustment",
    "non-demographic_adjustment",
    "activity_avoidance",
    "efficiencies",
    "bed_occupancy",
    "theatres"
  )

  # make sure to only select items that exist in the params
  p_order <- p_order[p_order %in% names(p)]
  # add in any items in the params that aren't in the order at the end
  p_order <- c(p_order, setdiff(names(p), p_order))

  p[p_order]
}
