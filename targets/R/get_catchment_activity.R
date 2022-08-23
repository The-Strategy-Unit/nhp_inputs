get_catchment_activity <- function(provider_successors, providers) {
  con <- get_con("HESData")

  provider_successors <- dbplyr::db_copy_to(
    con,
    "provider_successors",
    dplyr::select(
      provider_successors,
      .data$org_code,
      .data$succ_org_code
    )
  )

  providers <- dbplyr::db_copy_to(
    con,
    "providers",
    tibble::tibble(PROCODE3 = providers)
  )

  dplyr::tbl(con, dbplyr::in_schema("dbo", "tbInpatients")) |>
    dplyr::filter(
      .data$FYEAR >= 201516,
      .data$SPELEND == "Y",
      !is.na(.data$ADMIAGE),
      .data$LSOA11 %LIKE% "E%"
    ) |>
    dplyr::semi_join(dplyr::tbl(con, providers), by = c("PROCODE3")) |>
    dplyr::mutate(age_grp = ifelse(ADMIAGE >= 90, 90, floor(ADMIAGE / 5) * 5)) |>
    dplyr::left_join(dplyr::tbl(con, provider_successors), by = c("PROCODE3" = "org_code")) |>
    dplyr::group_by(
      .data$FYEAR,
      .data$LSOA11,
      .data$age_grp,
      .data$SEX
    ) |>
    dplyr::mutate(PROCODE3 = ifelse(is.na(.data$succ_org_code), .data$PROCODE3, .data$succ_org_code)) |>
    dplyr::count(PROCODE3) |>
    dplyr::mutate(
      tot = sum(n, na.rm = TRUE),
      p = n * 1.0 / tot
    ) |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across(
        .data$age_grp,
        \(.x) ifelse(
          .x >= 90,
          "90+",
          glue::glue("{stringr::str_pad(.x, 2, pad='0')}-{stringr::str_pad(.x + 4, 2, pad='0')}")
        )
      )
    ) |>
    janitor::clean_names()
}
