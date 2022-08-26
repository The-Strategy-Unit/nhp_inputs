get_provider_successors <- function(ods_successors, list_providers) {
  list_providers |>
    dplyr::left_join(ods_successors, by = c("PROCODE3" = "org_code")) |>
    dplyr::transmute(
      old_code = .data$PROCODE3,
      new_code = purrr::map2_chr(.data$succ_org_code, .data$PROCODE3, tidyr::replace_na)
    ) |>
    # TODO: address multiple successors
    dplyr::group_by(.data$old_code) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup()
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

  list(last_updated = Sys.time)
}
