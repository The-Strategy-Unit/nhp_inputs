save_data <- function(nhp_current_cohort, ...) {
  unlink("providers", TRUE, TRUE)

  provider_data <- list(...) |>
    purrr::imap(\(data, key) dplyr::group_nest(data, .data$procode, .data$strategy, .key = key)) |>
    purrr::reduce(dplyr::inner_join, by = c("procode", "strategy")) |>
    tidyr::pivot_longer(!where(rlang::is_atomic)) |>
    dplyr::filter(!purrr::map_lgl(.data$value, is.null)) |>
    dplyr::mutate(dplyr::across("value", \(.x) purrr::map(.x, janitor::remove_empty, "cols"))) |>
    dplyr::group_nest(.data$procode, .data$strategy) |>
    dplyr::mutate(dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))) |>
    dplyr::group_nest(.data$procode) |>
    dplyr::mutate(dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))) |>
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

get_provider_data <- function(age_sex_data, diagnoses_data, procedures_data, rates_data) {
  list(
    age_sex = age_sex_data,
    diagnoses = diagnoses_data,
    procedures_data = procedures_data,
    rates = rates_data
  ) |>
    purrr::imap(\(data, key) dplyr::group_nest(data, .data$procode, .data$strategy, .key = key)) |>
    purrr::reduce(dplyr::left_join, by = c("procode", "strategy")) |>
    dplyr::filter(!purrr::map_lgl(.data[["rates"]], is.null)) |>
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

get_wli_data <- function(ip_wli_data, op_wli_data, waiting_list_avg_change_data) {
  dplyr::full_join(
    ip_wli_data,
    op_wli_data,
    by = dplyr::join_by("fyear", "procode3", "tretspef")
  ) |>
    dplyr::inner_join(
      waiting_list_avg_change_data,
      by = dplyr::join_by("procode3" == "procode", "tretspef")
    ) |>
    dplyr::mutate(
      dplyr::across(
        c("ip", "op"),
        \(.x) tidyr::replace_na(.x, 0)
      )
    ) |>
    dplyr::group_nest(.data[["procode3"]], .data[["fyear"]]) |>
    dplyr::group_nest(.data[["procode3"]]) |>
    dplyr::mutate(
      dplyr::across(
        "data",
        \(.x) purrr::map(.x, purrr::compose(as.list, tibble::deframe))
      )
    ) |>
    tibble::deframe()
}

upload_data_to_azure <- function(provider,
                                 provider_data,
                                 expat_repat_data,
                                 covid_adjustment,
                                 wli_data,
                                 baseline_data,
                                 local = TRUE) {
  ep <- AzureStor::blob_endpoint(
    endpoint = Sys.getenv(ifelse(local, "LOCAL_STORAGE_EP", "AZ_STORAGE_EP")),
    key = Sys.getenv(ifelse(local, "LOCAL_STORAGE_KEY", "AZ_STORAGE_KEY"))
  )
  cont <- AzureStor::blob_container(ep, "inputs-data")

  upload_fn <- \(.x, name) {
    fn <- glue::glue("dev/{provider}/{name}.rds")
    AzureStor::storage_save_rds(.x, cont, fn)
    fn
  }

  list(
    files = c(
      upload_fn(provider_data[[provider]]$data, "data"),
      upload_fn(provider_data[[provider]]$available_strategies, "available_strategies"),
      upload_fn(expat_repat_data[[provider]], "expat_repat"),
      upload_fn(covid_adjustment[[provider]], "covid_adjustment"),
      upload_fn(wli_data[[provider]], "waiting_list_adjustment"),
      upload_fn(baseline_data[[provider]], "baseline_data")
    ),
    time = Sys.time()
  )
}

upload_reference_data_to_azure <- function(nhp_current_cohort, lkp_peers, local = TRUE) {
  ep <- AzureStor::blob_endpoint(
    endpoint = Sys.getenv(ifelse(local, "LOCAL_STORAGE_EP", "AZ_STORAGE_EP")),
    key = Sys.getenv(ifelse(local, "LOCAL_STORAGE_KEY", "AZ_STORAGE_KEY"))
  )
  cont <- AzureStor::blob_container(ep, "inputs-data")

  upload_fn <- \(.x, name) {
    fn <- glue::glue("dev/{name}.rds")
    AzureStor::storage_save_rds(.x, cont, fn)
    fn
  }

  upload_fn(nhp_current_cohort, "nhp_current_cohort")
  upload_fn(lkp_peers, "peers")
}
