save_data <- function(nhp_current_cohort, ...) {
  unlink("providers", TRUE, TRUE)

  provider_data <- list(...) |>
    purrr::imap(\(data, key) dplyr::group_nest(data, .data$procode, .data$strategy, .key = key)) |>
    purrr::reduce(dplyr::inner_join, by = c("procode", "strategy")) |>
    tidyr::pivot_longer(!where(rlang::is_atomic)) |>
    dplyr::filter(!purrr::map_lgl(.data$value, is.null)) |>
    dplyr::mutate(dplyr::across("value", purrr::map, janitor::remove_empty, "cols")) |>
    dplyr::group_nest(.data$procode, .data$strategy) |>
    dplyr::mutate(dplyr::across("data", purrr::map, tibble::deframe)) |>
    dplyr::group_nest(.data$procode) |>
    dplyr::mutate(dplyr::across("data", purrr::map, tibble::deframe)) |>
    dplyr::filter(.data$procode %in% nhp_current_cohort) |>
    tibble::deframe()

  available_strategies <- provider_data |>
    purrr::imap(\(.x, .i) {
      .x |>
        purrr::map_dfr("rates", .id = "strategy") |>
        dplyr::filter(.data$peer == .i) |>
        tidyr::drop_na("rate") |>
        dplyr::group_by(dplyr::across("fyear", as.character)) |>
        dplyr::summarise(dplyr::across("strategy", list), .groups = "drop") |>
        tibble::deframe()
    })

  saveRDS(provider_data, "provider_data.Rds")
  saveRDS(available_strategies, "available_strategies.Rds")

  list("save_data", Sys.time())
}

get_provider_data <- function(age_sex_data, diagnoses_data, rates_data) {
  list(
    age_sex = age_sex_data,
    diagnoses = diagnoses_data,
    rates = rates_data
  ) |>
    purrr::imap(\(data, key) dplyr::group_nest(data, .data$procode, .data$strategy, .key = key)) |>
    purrr::reduce(dplyr::inner_join, by = c("procode", "strategy")) |>
    tidyr::pivot_longer(!where(rlang::is_atomic)) |>
    dplyr::filter(!purrr::map_lgl(.data$value, is.null)) |>
    dplyr::mutate(dplyr::across("value", \(.x) purrr::map(.x, janitor::remove_empty, "cols"))) |>
    dplyr::group_nest(.data$procode, .data$strategy) |>
    dplyr::mutate(dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))) |>
    dplyr::group_nest(.data$procode) |>
    dplyr::mutate(dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))) |>
    dplyr::mutate(
      available_strategies = purrr::map2(
        .data$procode,
        .data$data,
        \(.i, .x) {
          .x |>
            purrr::map_dfr("rates", .id = "strategy") |>
            dplyr::filter(.data$peer == .i) |>
            tidyr::drop_na("rate") |>
            dplyr::group_by(dplyr::across("fyear", as.character)) |>
            dplyr::summarise(dplyr::across("strategy", list), .groups = "drop") |>
            tibble::deframe()
        }
      )
    ) |>
    tidyr::pivot_longer(-"procode") |>
    dplyr::group_nest(.data$procode) |>
    dplyr::mutate(dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))) |>
    tibble::deframe()
}

upload_data_to_azure <- function(provider, provider_data, expat_repat_data, covid_adjustment) {
  ep <- AzureStor::adls_endpoint(
    endpoint = Sys.getenv("TARGETS_AZURE_SA_EP"),
    key = Sys.getenv("TARGETS_AZURE_SA_key")
  )
  fs <- AzureStor::adls_filesystem(ep, "inputs-data")

  upload_fn <- \(.x, name) {
    fn <- glue::glue("{provider}/{name}.rds")
    tf <- withr::local_tempfile()
    saveRDS(.x, tf)
    AzureStor::upload_adls_file(fs, tf, fn)
    fn
  }

  list(
    files = c(
      upload_fn(provider_data[[provider]]$data, "data"),
      upload_fn(provider_data[[provider]]$available_strategies, "available_strategies"),
      upload_fn(expat_repat_data[[provider]], "expat_repat"),
      upload_fn(covid_adjustment[[provider]], "covid_adjustment")
    ),
    time = Sys.time()
  )
}

upload_reference_data_to_azure <- function(nhp_current_cohort, lkp_peers) {
  ep <- AzureStor::adls_endpoint(
    endpoint = Sys.getenv("TARGETS_AZURE_SA_EP"),
    key = Sys.getenv("TARGETS_AZURE_SA_key")
  )
  fs <- AzureStor::adls_filesystem(ep, "inputs-data")

  upload_fn <- \(.x, name) {
    fn <- glue::glue("{name}.rds")
    tf <- withr::local_tempfile()
    saveRDS(.x, tf)
    AzureStor::upload_adls_file(fs, tf, fn)
    fn
  }

  upload_fn(nhp_current_cohort, "nhp_current_cohort")
  upload_fn(lkp_peers, "peers")
}
