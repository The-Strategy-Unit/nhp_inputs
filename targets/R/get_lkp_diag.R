get_lkp_diag <- function() {
  con <- get_con("Reference")

  icd_diags <- dplyr::tbl(con, "DIM_tbDiagnosis") |>
    dplyr::select(
      diagnosis_code = "DiagnosisCode",
      diagnosis_description = "DiagnosisDescription"
    ) |>
    dplyr::filter(.data$diagnosis_code %LIKE% "%X" | .data$diagnosis_code %LIKE% "___") |>
    dplyr::collect() |>
    dplyr::arrange(.data$diagnosis_code) |>
    dplyr::group_by(dplyr::across("diagnosis_code", \(.x) stringr::str_sub(.x, 1, 3))) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  aae_diags <- dplyr::tbl(con, "DIM_tbAEDiagnosis") |>
    dplyr::filter(.data$AESubDiagnosisCode == "") |>
    dplyr::select(
      diagnosis_code = "AEDiagnosisCode",
      diagnosis_description = "AEDiagnosisDescription"
    ) |>
    dplyr::collect()

  dplyr::bind_rows(
    "icd10" = icd_diags,
    "aae" = aae_diags,
    .id = "type"
  )
}
