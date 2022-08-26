get_ods_succ_update_date <- function() {
  page <- rvest::read_html("https://digital.nhs.uk/services/organisation-data-service/file-downloads/miscellaneous")

  page |>
    rvest::html_elements("table tr") |>
    purrr::keep(~ rvest::html_text(.x) |> stringr::str_starts("(Archived\\s*)?Successor")) |>
    purrr::map(rvest::html_children) |>
    purrr::map(3) |>
    purrr::map_chr(rvest::html_text) |>
    lubridate::as_date(format = "%d %b %Y") |>
    max()
}
