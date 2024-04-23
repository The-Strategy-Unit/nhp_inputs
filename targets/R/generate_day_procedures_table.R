.data <- rlang::.data
.env <- rlang::.env

get_day_procedures_counts <- function() {
  con <- get_con("HESData")

  tbl_ip <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::filter(
      .data[["ADMIMETH"]] %LIKE% "1%", # nolint
      .data[["FYEAR"]] == 201920,
      .data[["CLASSPAT"]] %in% c("1", "2")
    )

  tbl_ipp <- dplyr::tbl(con, "tbInpatientsProcedures") |>
    dplyr::filter(
      .data[["OPORDER"]] == 1,
      .data[["OPCODE"]] %LIKE% "[A-T][0-9][0-9][0-9]%" # nolint
    )

  tbl_op <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "outpatients")) |>
    dplyr::filter(
      .data[["fyear"]] == 201920,
      !.data[["sushrg"]] %LIKE% "WF%", # nolint
      !.data[["sushrg"]] %LIKE% "U%" # nolint
    )

  tbl_opp <- dplyr::tbl(con, "tbOutpatientsProcedures") |>
    dplyr::filter(
      .data[["oporder"]] == 1,
      .data[["opcode"]] %LIKE% "[A-T][0-9][0-9][0-9]%" # nolint
    )

  df_op <- tbl_op |>
    dplyr::inner_join(tbl_opp, by = dplyr::join_by("attendkey")) |>
    dplyr::count(procedure_code = .data[["opcode"]]) |>
    dplyr::collect() |>
    dplyr::mutate(
      type = "op"
    )

  df_ip <- tbl_ip |>
    dplyr::inner_join(tbl_ipp, by = dplyr::join_by("EPIKEY")) |>
    dplyr::count(.data[["CLASSPAT"]], .data[["OPCODE"]]) |>
    dplyr::collect() |>
    dplyr::transmute(
      type = ifelse(.data[["CLASSPAT"]] == "1", "ip", "dc"),
      procedure_code = .data[["OPCODE"]],
      .data[["n"]]
    )

  dplyr::bind_rows(df_op, df_ip) |>
    tidyr::pivot_wider(
      names_from = .data[["type"]],
      values_from = .data[["n"]],
      values_fill = 0
    ) |>
    dplyr::arrange(.data[["procedure_code"]]) |>
    dplyr::mutate(
      total = .data[["op"]] + .data[["dc"]] + .data[["ip"]]
    )
}

generate_day_procedures_table <- function(day_procedures_counts, p_value) {
  day_procedures_counts <- dplyr::filter(
    day_procedures_counts,
    .data[["total"]] >= 100
  )

  fn <- function(col, p) {
    day_procedures_counts |>
      dplyr::filter(
        purrr::map2(
          .data[[col]],
          .data[["total"]],
          binom.test,
          p = p,
          alternative = "greater"
        ) |>
          purrr::map_dbl("p.value") < p_value
      ) |>
      dplyr::select("procedure_code")
  }

  dfs <- list(
    usually_op = fn("op", 0.5),
    usually_dc = fn("dc", 0.5)
  )

  dfs$occasionally_op <- fn("op", 0.05) |>
    dplyr::anti_join(dfs$usually_op, by = dplyr::join_by("procedure_code")) |>
    dplyr::anti_join(dfs$usually_dc, by = dplyr::join_by("procedure_code"))

  dfs$occasionally_dc <- fn("dc", 0.05) |>
    dplyr::anti_join(dfs$usually_op, by = dplyr::join_by("procedure_code")) |>
    dplyr::anti_join(dfs$usually_dc, by = dplyr::join_by("procedure_code"))

  day_procedures_table <- dplyr::bind_rows(dfs, .id = "type")


  con <- get_con("HESData")
  dplyr::copy_to(
    con,
    day_procedures_table,
    dbplyr::in_schema("nhp_modelling_reference", "day_procedure_opcs_codes"),
    overwrite = TRUE,
    temporary = FALSE
  )

  day_procedures_table
}


#   purrr::reduce(
#     .init = df_raw,
#     list(
#       transmute(usually_op, procedure_code, usually_op = TRUE),
#       transmute(usually_dc, procedure_code, usually_dc = TRUE),
#       transmute(occasionally_op, procedure_code, occasionally_op = TRUE),
#       transmute(occasionally_dc, procedure_code, occasionally_dc = TRUE)
#     ),
#     dplyr::left_join,
#     by = "procedure_code"
#   ) |>
#     dplyr::mutate(
#       dplyr::across(
#         tidyselect::matches("^(usually|occasionally)"),
#         \(.x) tidyr::replace_na(.x, FALSE)
#       )
#     ) |>
#     dplyr::count(
#       dplyr::across(
#         tidyselect::matches("^(usually|occasionally)")
#       )
#     ) |>
#     print()
