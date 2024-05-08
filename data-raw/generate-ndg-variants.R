ndg_variants <- list(
  variant_1 = list(
    aae = list(
      ambulance = c(1.0121, 1.0142),
      "walk-in" = c(1.0121,	1.0142)
    ),
    ip = list(
      elective = c(1.0053, 1.0071),
      maternity = c(1, 1),
      "non-elective" = c(1.0194, 1.0240)
    ),
    op = list(
      first = c(1.0228, 1.0270),
      followup = c(1.0228, 1.0270),
      procedure = c(1.0228, 1.0270)
    )
  ),
  variant_2 = list(
    aae = list(
      ambulance = c(1.0121, 1.0142),
      "walk-in" = c(1.0121,	1.0142)
    ),
    ip = list(
      elective = c(1.0053, 1.0071),
      maternity = c(1, 1),
      "non-elective" = c(0.9955, 1.0017)
    ),
    op = list(
      first = c(1.0228, 1.0270),
      followup = c(1.0228, 1.0270),
      procedure = c(1.0228, 1.0270)
    )
  )
)

readr::write_rds(ndg_variants, "inst/app/data/ndg_variants.rds")
