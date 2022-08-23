get_lkp_diag <- function() {
  con <- get_con("Reference")

  dplyr::tbl(con, dbplyr::in_schema("dbo", "DIM_tbDiagnosis")) |>
    dplyr::select(
      diagnosis_code = .data$DiagnosisCode,
      diagnosis_description = .data$DiagnosisDescription
    ) |>
    dplyr::collect()
}
