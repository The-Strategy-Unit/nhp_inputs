get_baseline_data <- function(all_providers, ...) {
  dplyr::bind_rows(
    ip = get_baseline_data_ip(),
    op = get_baseline_data_op(),
    aae = get_baseline_data_aae(),
    .id = "activity_type"
  ) |>
    dplyr::filter(.data[["procode3"]] %in% all_providers) |>
    tidyr::nest(.by = c("procode3", "fyear")) |>
    tidyr::nest(.by = c("procode3")) |>
    tibble::deframe() |>
    purrr::map(tibble::deframe)
}

get_baseline_data_x <- function(file, fn) {
  providers <- c(
    fs::dir_ls("../nhp_model/data/2019"),
    fs::dir_ls("../nhp_model/data/2022")
  )

  providers |>
    file.path(file) |>
    purrr::set_names(stringr::str_extract(providers, "(?<=/)\\w*$")) |>
    purrr::keep(file.exists) |>
    purrr::map(\(.x) {
      fyear <- stringr::str_extract(.x, "(?<=/)\\d{4}(?=/)") |>
        as.integer() |>
        (\(.y) .y * 100 + (.y + 1) %% 100)()

      arrow::read_parquet(.x) |>
        fn() |>
        dplyr::mutate(fyear = fyear, .before = tidyselect::everything())
    }) |>
    purrr::discard(\(.x) nrow(.x) == 0) |>
    dplyr::bind_rows(.id = "procode3")
}

get_baseline_data_ip <- function() {
  get_baseline_data_x(
    "ip.parquet",
    \(.x) dplyr::count(.x, .data[["group"]], .data[["tretspef"]])
  )
}

get_baseline_data_op <- function() {
  get_baseline_data_x(
    "op.parquet",
    \(.x) {
      dplyr::summarise(
        .x,
        n = sum(.data[["attendances"]] + .data[["tele_attendances"]]),
        .by = c("group", "tretspef")
      )
    }
  )
}

get_baseline_data_aae <- function() {
  get_baseline_data_x(
    "aae.parquet",
    \(.x) {
      dplyr::count(
        .x,
        .data[["group"]],
        wt = .data[["arrivals"]]
      )
    }
  )
}
