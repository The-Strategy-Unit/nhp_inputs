get_lkp_peers <- function(lkp_peers_file, provider_successors) {
  lkp_peers <- readxl::read_excel(lkp_peers_file, sheet = "Default peers") |>
    janitor::remove_empty(which = "cols") |>
    janitor::clean_names() |>
    dplyr::inner_join(provider_successors, by = c("procode" = "old_code")) |>
    dplyr::inner_join(provider_successors, by = c("peer" = "old_code")) |>
    dplyr::select(procode = .data$new_code.x, .data$new_code.y)
}
