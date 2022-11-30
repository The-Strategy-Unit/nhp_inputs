get_ccg_to_icb_lu <- function(ods_successors, icb_lu_path) {
  con <- get_con("UK_Health_Dimensions")

  ccgs <- dplyr::tbl(con, dbplyr::in_schema("ODS", "Clinical_Commissioning_Groups_SCD")) |>
    dplyr::distinct(ccg = .data$Organisation_Code) |>
    dplyr::collect()

  find_current <- function(v, graph) {
    n <- igraph::neighbors(graph, v)$name
    # if we find no neighbours then we simply return the vertex that we are at
    if (length(n) == 0) {
      return(v)
    }
    # recursively call this function to find the current ccg
    find_current(n, graph)
  }

  ccg_to_loc22cdh <- ods_successors |>
    dplyr::semi_join(ccgs, by = c("org_code" = "ccg")) |>
    igraph::graph_from_data_frame(vertices = ccgs) |>
    tidygraph::as_tbl_graph() |>
    dplyr::mutate(loc22cdh = purrr::map_chr(.data$name, find_current, graph = tidygraph::.G())) |>
    dplyr::as_tibble() |>
    dplyr::rename(ccg = "name")

  readxl::read_excel(icb_lu_path) |>
    janitor::clean_names() |>
    dplyr::select("loc22cdh", "icb22cdh") |>
    dplyr::inner_join(ccg_to_loc22cdh, by = c("loc22cdh")) |>
    dplyr::relocate("ccg")
}

upload_ccg_to_icb_lu <- function(ccg_to_icb_lu) {
  con <- get_con("HESData")

  dplyr::copy_to(
    con,
    ccg_to_icb_lu,
    name = DBI::Id(schema = "nhp_modelling_reference", table = "ccg_to_icb"),
    overwrite = TRUE,
    temporary = FALSE
  )

  Sys.time()
}
