lookup_file_path <- function(file) {
  app_sys("app", "reference", file)
}

get_diagnoses_lookup <- function() {
  readr::read_csv(
    lookup_file_path("diagnoses.csv"),
    col_types = "ccc",
    progress = FALSE
  )
}

get_procedures_lookup <- function() {
  readr::read_csv(
    lookup_file_path("procedures.csv"),
    col_types = "ccc",
    progress = FALSE
  )
}

get_mitigators_lookup <- function() {
  lookup_file_path("mitigator-codes.csv") |>
    readr::read_csv(col_types = "c", progress = FALSE) |>
    dplyr::mutate(
      strategy_name_full = glue::glue("{strategy_name} ({mitigator_code})")
    )
}

get_peers_lookup <- function() {
  readr::read_csv(
    lookup_file_path("peers.csv"),
    col_types = "cc",
    progress = FALSE
  )
}

get_providers_lookup <- function() {
  lookup_file_path("providers.csv") |>
    readr::read_csv(col_types = "cc", progress = FALSE) |>
    tibble::deframe()
}

get_ndg_variants_lookup <- function() {
  lookup_file_path("ndg_variants.json") |>
    jsonlite::read_json(simplifyVector = TRUE) |>
    purrr::keep_at(c("variant_2", "variant_3"))
}

get_nee_lookup <- function() {
  readr::read_csv(
    lookup_file_path("nee_table.csv"),
    col_types = "cddd",
    progress = FALSE
  )
}

get_rtt_specialties_lookup <- function() {
  lookup_file_path("rtt_specialties.csv") |>
    readr::read_csv(col_types = "cc", progress = FALSE) |>
    dplyr::mutate(sanitized_code = sanitize_input_name(.data[["code"]]))
}

get_waiting_list_multipliers <- function() {
  lookup_file_path("waiting_list_params.csv") |>
    readr::read_csv(col_types = "cddddd", progress = FALSE) |>
    dplyr::transmute(
      .data[["tretspef"]],
      ip = .data[["mixed_split"]] *
        .data[["avg_ip_activity_per_pathway_mixed"]],
      op = .data[["op_only_split"]] *
        .data[["avg_op_first_activity_per_pathway_op_only"]] +
        .data[["mixed_split"]] *
          .data[["avg_op_first_activity_per_pathway_mixed"]]
    ) |>
    tidyr::pivot_longer(
      c("ip", "op"),
      names_to = "activity_type",
      values_to = "multiplier"
    )
}

get_icb_boundaries <- function() {
  sf::read_sf(lookup_file_path("icb_boundaries.geojson"))
}

# use a singleton pattern to cache lookups in memory, but prevent the files from
# being read immediately when the package is attached
.lookups_cache <- new.env()

get_lookups <- function() {
  if (length(ls(envir = .lookups_cache)) == 0) {
    list2env(
      list(
        "diagnoses" = get_diagnoses_lookup(),
        "procedures" = get_procedures_lookup(),
        "mitigators" = get_mitigators_lookup(),
        "peers" = get_peers_lookup(),
        "providers" = get_providers_lookup(),
        "ndg_variants" = get_ndg_variants_lookup(),
        "nee_table" = get_nee_lookup(),
        "rtt_specialties" = get_rtt_specialties_lookup(),
        "waiting_list_multipliers" = get_waiting_list_multipliers(),
        "icb_boundaries" = get_icb_boundaries()
      ),
      envir = .lookups_cache
    )
  }
  .lookups_cache
}
