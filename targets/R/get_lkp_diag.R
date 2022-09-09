get_lkp_diag <- function() {
  con <- get_con("Reference")

  diags <- dplyr::tbl(con, dbplyr::in_schema("dbo", "DIM_tbDiagnosis")) |>
    dplyr::select(
      diagnosis_code = .data$DiagnosisCode,
      diagnosis_description = .data$DiagnosisDescription
    ) |>
    dplyr::filter(.data$diagnosis_code %LIKE% "%X" | .data$diagnosis_code %LIKE% "___") |>
    dplyr::collect()

  diags |>
    dplyr::arrange(.data$diagnosis_code) |>
    dplyr::group_by(across(.data$diagnosis_code, stringr::str_sub, 1, 3)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}
