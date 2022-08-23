get_ods_successors <- function(last_update_date) {
  purrr::map_dfr(c("succ", "succarc"), \(.x) {
    tf <- withr::local_tempfile()
    file <- glue::glue("{.x}.csv")
    withr::local_file(file)

    url <- glue::glue("https://files.digital.nhs.uk/assets/ods/current/{.x}.zip")
    download.file(url, tf, mode = "wb")

    unzip(tf, files = file)
    readr::read_csv(
      file,
      col_names = c("org_code", "succ_org_code", "reason", "date", "indicator"),
      col_types = c("cc_c_")
    ) |>
      dplyr::mutate(dplyr::across(.data$date, lubridate::as_date))
  })
}
