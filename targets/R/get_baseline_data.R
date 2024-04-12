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

get_baseline_data_ip <- function() {
  providers <- fs::dir_ls("../nhp_model/data/2019")

  providers |>
    file.path("ip.parquet") |>
    purrr::set_names(stringr::str_extract(providers, "(?<=/)\\w*$")) |>
    purrr::keep(file.exists) |>
    purrr::map(\(.x) {
      arrow::read_parquet(.x) |>
        dplyr::filter(
          !.data[["bedday_rows"]]
        ) |>
        dplyr::count(fyear = 201920, .data[["group"]], .data[["tretspef"]])
    }) |>
    dplyr::bind_rows(.id = "procode3")
}

get_baseline_data_op <- function() {
  providers <- fs::dir_ls("../nhp_model/data/2019")

  providers |>
    file.path("op.parquet") |>
    purrr::set_names(stringr::str_extract(providers, "(?<=/)\\w*$")) |>
    purrr::keep(file.exists) |>
    purrr::map(\(.x) {
      arrow::read_parquet(.x) |>
        dplyr::summarise(
          fyear = 201920,
          n = sum(.data[["attendances"]] + .data[["tele_attendances"]]),
          .by = c("group", "tretspef")
        )
    }) |>
    dplyr::bind_rows(.id = "procode3")
}

get_baseline_data_aae <- function() {
  providers <- fs::dir_ls("../nhp_model/data/2019")

  providers |>
    file.path("aae.parquet") |>
    purrr::set_names(stringr::str_extract(providers, "(?<=/)\\w*$")) |>
    purrr::keep(file.exists) |>
    purrr::map(\(.x) {
      arrow::read_parquet(.x) |>
        dplyr::count(
          fyear = 201920,
          .data[["group"]],
          wt = .data[["arrivals"]]
        )
    }) |>
    purrr::discard(\(.x) nrow(.x) == 0) |>
    dplyr::bind_rows(.id = "procode3")
}
