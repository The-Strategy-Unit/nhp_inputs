library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

dir("targets/R", pattern = ".R", full.names = TRUE) |>
  sapply(source) |>
  invisible()

# Set target-specific options such as packages.
tar_option_set(packages = "dplyr")

# End this file with a list of target objects.
list(
  tar_target(
    rtt_specialties,
    c(
      `General Surgery` = "100",
      `Urology` = "101",
      `Trauma and Orthopaedics` = "110",
      `Ear Nose and Throat` = "120",
      `Ophthalmology` = "130",
      `Oral Surgery` = "140",
      `Neurosurgery` = "150",
      `Plastic Surgery` = "160",
      `Cardiothoracic Surgery` = "170",
      `General Internal Medicine` = "300",
      `Gastroenterology` = "301",
      `Cardiology` = "320",
      `Dermatology` = "330",
      `Respiratory Medicine` = "340",
      `Neurology Service` = "400",
      `Rheumatology Service` = "410",
      `Elderly Medicine Service` = "430",
      `Gynaecology Service` = "502",
      `Other (Medical)` = "Other (Medical)",
      `Other (Surgical)` = "Other (Surgical)",
      `Other` = "Other"
    )
  ),
  tar_target(age_table, get_age_table()),
  tar_target(lkp_diag, get_lkp_diag()),
  tar_target(lkp_provider_names, get_lkp_provider_names()),
  tar_target(lkp_provider_names2, get_lkp_provider_names2()),
  tar_target(lkp_euro_2013, get_lkp_euro_2013()),
  tar_target(list_providers_file, "targets/data/list_providers.rds", format = "file"),
  tar_target(list_providers, readRDS(list_providers_file)),
  tar_target(ods_succ_update_date, get_ods_succ_update_date(), cue = targets::tar_cue("always")),
  tar_target(ods_successors, get_ods_successors(ods_succ_update_date)),
  tar_target(provider_successors, get_provider_successors(ods_successors, list_providers)),
  tar_target(provider_successors_last_updated, upload_provider_successors(provider_successors)),
  tar_target(lkp_peers_file, "targets/data/Peer+finder+Appendix+A.xlsx", format = "file"),
  tar_target(lkp_peers, get_lkp_peers(lkp_peers_file, provider_successors)),
  tar_target(providers, get_providers(lkp_peers, lkp_provider_names2, lkp_provider_names)),
  tar_target(provider_locations, get_provider_locations(providers)),
  tar_target(pop_year_long, get_pop_year_long(age_table)),
  tar_target(catchments, get_catchments(provider_successors_last_updated, pop_year_long)),
  tar_target(icb_lu_path, "targets/data/LOC22_ICB22_NHSER22_EN_LU.xlsx", format = "file"),
  tar_target(icb_code_lu_path, "targets/data/icb22cdh_to_icb22cd.csv", format = "file"),
  tar_target(icb_boundaries, get_icb_boundaries(icb_code_lu_path)),
  tar_target(ccg_to_icb_lu, get_ccg_to_icb_lu(ods_successors, icb_lu_path)),
  tar_target(ccg_to_icb_last_updated, upload_ccg_to_icb_lu(ccg_to_icb_lu)),
  # ip data
  tar_target(strategies_last_updated, Sys.Date()), # use tar_invalidate(strategies_last_updated)
  tar_target(strategies, get_strategies(strategies_last_updated)),
  tar_target(ip_age_sex_data, get_ip_age_sex_data(strategies_last_updated, provider_successors_last_updated)),
  tar_target(ip_diag_data, get_ip_diag_data(strategies_last_updated, provider_successors_last_updated)),
  tar_target(ip_los_data, get_ip_los_data(strategies_last_updated, provider_successors_last_updated)),
  tar_target(
    ip_wli_data,
    get_ip_wli_data(
      strategies_last_updated,
      provider_successors_last_updated,
      rtt_specialties
    )
  ),
  tar_target(
    expat_ip_data,
    get_expat_ip_data(
      rtt_specialties,
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  tar_target(
    repat_local_ip_data,
    get_repat_local_ip_data(
      rtt_specialties,
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  tar_target(
    repat_nonlocal_ip_data,
    get_repat_nonlocal_ip_data(
      rtt_specialties,
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  # op data
  tar_target(op_data, get_op_data(provider_successors_last_updated)),
  tar_target(op_diag_data, get_op_diag_data(provider_successors_last_updated)),
  tar_target(op_age_sex_data, get_op_age_sex_data(op_data)),
  tar_target(
    op_wli_data,
    get_op_wli_data(
      provider_successors_last_updated,
      rtt_specialties
    )
  ),
  tar_target(
    expat_op_data,
    get_expat_op_data(
      rtt_specialties,
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  tar_target(
    repat_local_op_data,
    get_repat_local_op_data(
      rtt_specialties,
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  tar_target(
    repat_nonlocal_op_data,
    get_repat_nonlocal_op_data(
      rtt_specialties,
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  # aae data
  tar_target(aae_data, get_aae_data(provider_successors_last_updated)),
  tar_target(aae_diag_data, get_aae_diag_data(provider_successors_last_updated)),
  tar_target(aae_age_sex_data, get_aae_age_sex_data(aae_data)),
  tar_target(
    expat_aae_data,
    get_expat_aae_data(
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  tar_target(
    repat_local_aae_data,
    get_repat_local_aae_data(
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  tar_target(
    repat_nonlocal_aae_data,
    get_repat_nonlocal_aae_data(
      provider_successors_last_updated,
      ccg_to_icb_last_updated
    )
  ),
  # rates
  tar_target(ip_dsr_data, get_ip_dsr_data(ip_age_sex_data, lkp_peers, catchments, lkp_euro_2013, strategies)),
  tar_target(mean_los_data, get_mean_los_data(ip_los_data, lkp_peers)),
  tar_target(zero_los_data, get_zero_los_data(ip_los_data, lkp_peers)),
  tar_target(preop_los_data, get_preop_los_data(ip_los_data, lkp_peers)),
  tar_target(bads_data, get_bads_data(ip_los_data, lkp_peers)),
  tar_target(op_convert_to_tele_data, get_op_convert_to_tele_data(op_data, lkp_peers)),
  tar_target(op_consultant_to_consultant_reduction, get_op_consultant_to_consultant_reduction(op_data, lkp_peers)),
  tar_target(op_followup_reduction, get_op_followup_reduction(op_data, lkp_peers)),
  tar_target(aae_rates, get_aae_rates(aae_data, lkp_peers)),
  # repat/expat data
  tar_target(
    expat_repat_data,
    get_expat_repat_data(
      expat_ip_data,
      expat_op_data,
      expat_aae_data,
      repat_local_ip_data,
      repat_local_op_data,
      repat_local_aae_data,
      repat_nonlocal_ip_data,
      repat_nonlocal_op_data,
      repat_nonlocal_aae_data
    )
  ),
  # covid adjustments
  tar_target(
    covid_adjustment_data_ip,
    get_covid_adjustment_data_ip(strategies_last_updated, provider_successors_last_updated)
  ),
  tar_target(
    covid_adjustment_data_op,
    get_covid_adjustment_data_op(strategies_last_updated, provider_successors_last_updated)
  ),
  tar_target(
    covid_adjustment_data_aae,
    get_covid_adjustment_data_aae(strategies_last_updated, provider_successors_last_updated)
  ),
  tar_target(
    covid_adjustment,
    get_covid_adjustment(
      covid_adjustment_data_ip,
      covid_adjustment_data_op,
      covid_adjustment_data_aae
    )
  ),
  # wli data
  tar_target(
    wli_data,
    get_wli_data(ip_wli_data, op_wli_data)
  ),
  # save data
  tar_target(
    nhp_current_cohort,
    c(
      "RA9",
      "RD8",
      "RGP",
      "RGR",
      "RH5", # "RBA" is merged in with this activity
      "RH8", # was "RBZ",
      "RHW",
      "RN5",
      "RNQ",
      "RX1",
      "RXC",
      "RXN", # need to merge in "RTX"
      "RYJ"
    )
  ),
  # combined data into single items
  tar_target(
    age_sex_data,
    dplyr::bind_rows(
      ip_age_sex_data,
      op_age_sex_data,
      aae_age_sex_data
    )
  ),
  tar_target(
    diagnoses_data,
    dplyr::bind_rows(
      ip_diag_data,
      op_diag_data,
      aae_diag_data
    )
  ),
  tar_target(
    rates_data,
    dplyr::bind_rows(
      ip_dsr_data,
      mean_los_data,
      zero_los_data,
      preop_los_data,
      bads_data,
      op_convert_to_tele_data,
      op_consultant_to_consultant_reduction,
      op_followup_reduction,
      aae_rates
    )
  ),
  tar_target(
    provider_data,
    get_provider_data(age_sex_data, diagnoses_data, rates_data)
  ),
  tar_target(
    uploaded_data_azure,
    upload_data_to_azure(
      nhp_current_cohort,
      provider_data,
      expat_repat_data,
      covid_adjustment,
      wli_data,
      Sys.getenv("AZ_STORAGE_EP"),
      Sys.getenv("AZ_STORAGE_KEY")
    ),
    pattern = map(nhp_current_cohort)
  ),
  tar_target(
    uploaded_data_local,
    upload_data_to_azure(
      nhp_current_cohort,
      provider_data,
      expat_repat_data,
      covid_adjustment,
      wli_data,
      Sys.getenv("LOCAL_STORAGE_EP"),
      Sys.getenv("LOCAL_STORAGE_KEY")
    ),
    pattern = map(nhp_current_cohort)
  ),
  tar_target(
    uploaded_reference_data_azure,
    upload_reference_data_to_azure(
      nhp_current_cohort,
      lkp_peers,
      Sys.getenv("AZ_STORAGE_EP"),
      Sys.getenv("AZ_STORAGE_KEY")
    )
  ),
  tar_target(
    uploaded_reference_data_local,
    upload_reference_data_to_azure(
      nhp_current_cohort,
      lkp_peers,
      Sys.getenv("LOCAL_STORAGE_EP"),
      Sys.getenv("LOCAL_STORAGE_KEY")
    )
  ),
  tar_target(reference_data_last_updated, {
    dir.create("inst/app/data", FALSE)

    withr::with_dir("inst/app/data", {
      saveRDS(rtt_specialties, "rtt_specialties.Rds")
      saveRDS(lkp_diag, "diagnoses.Rds")
      saveRDS(providers, "providers.Rds")
      sf::write_sf(provider_locations, "provider_locations.geojson", delete_dsn = TRUE)
      sf::write_sf(icb_boundaries, "icb_boundaries.geojson", delete_dsn = TRUE)
    })

    Sys.time()
  })
)
