get_provider_successors <- function(ods_successors, list_providers) {
  successors <- list_providers |>
    dplyr::left_join(ods_successors, by = c("PROCODE3" = "org_code")) |>
    dplyr::transmute(
      old_code = .data$PROCODE3,
      new_code = purrr::map2_chr(.data$succ_org_code, .data$PROCODE3, tidyr::replace_na)
    ) |>
    # remove duplicates:
    # Penine Acute Hospitals NHS Trust -> Manchester Univesity NHS FT
    #   (most activity should be mapped to Northern Care Allicance NHS FT)
    dplyr::filter(!(.data$old_code == "RW6" & .data$new_code == "R0A")) |>
    # update successors: (add in any missing successors from the ODS file)
    dplyr::rows_update(
      tibble::tribble(
        ~old_code, ~new_code,
        "RBA", "RH5", # missing row for Taunton and Somerset NHS FT -> Somerset NHS FT
        "RA3", "RA7", # missing row for Weston Area Health NHS Trust -> Univesity Hospitals Bristol and Weston NHS FT
        "RBZ", "RH8" # missing row for Northern Devon Healthcare NHS Trust -> Royal Devon University Healthcare NHS FT
      ),
      by = "old_code"
    )

  missing_rows <- dplyr::anti_join(
    successors,
    successors,
    by = dplyr::join_by("new_code" == "old_code")
  ) |>
    dplyr::distinct(old_code = .data[["new_code"]], .data[["new_code"]])

  successors <- dplyr::bind_rows(successors, missing_rows)

  stopifnot(
    "non-unique mappings in successors file" = successors |>
      dplyr::group_by(.data$old_code) |>
      dplyr::filter(dplyr::n() > 1) |>
      nrow() == 0
  )

  successors
}

upload_provider_successors <- function(provider_successors) {
  con <- get_con("HESData")

  DBI::dbWriteTable(
    con,
    DBI::Id(schema = "nhp_modelling", table = "provider_successors"),
    provider_successors,
    overwrite = TRUE,
    temporary = FALSE,
    field.types = c(
      "old_code" = "NVARCHAR(5) NOT NULL",
      "new_code" = "NVARCHAR(5) NOT NULL"
    )
  )

  DBI::dbExecute(
    con,
    "

    ALTER TABLE
      nhp_modelling.provider_successors
      ALTER COLUMN old_code NVARCHAR(5) NOT NULL;

    ALTER TABLE
      nhp_modelling.provider_successors
      ALTER COLUMN new_code NVARCHAR(5) NOT NULL;

    ALTER TABLE nhp_modelling.provider_successors
        ADD CONSTRAINT PK_nhp_modelling_provider_successors
        PRIMARY KEY CLUSTERED (old_code);

    "
  )

  list(last_updated = Sys.time())
}
