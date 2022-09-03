get_lkp_peers <- function(lkp_peers_file, provider_successors) {
  readxl::read_excel(lkp_peers_file, sheet = "Default peers") |>
    janitor::remove_empty(which = "cols") |>
    janitor::clean_names() |>
    # ignore any record where the procode field has been succeeded, we will just use the succeeded orgs peers
    # dplyr::inner_join(provider_successors, by = c("procode" = "old_code")) |>
    dplyr::left_join(provider_successors, by = c("peer" = "old_code")) |>
    dplyr::transmute(
      .data$procode,
      peer = purrr::map2_chr(.data$new_code, .data$peer, tidyr::replace_na)
    ) |>
    # if a peer was succeeded, we will have less rows of data, but we need to remove duplicates
    dplyr::distinct(dplyr::across(tidyselect::everything()))
}
