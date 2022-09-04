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
  tar_target(pop_year_long, get_pop_year_long(age_table)),
  tar_target(catchments, get_catchments(provider_successors_last_updated, pop_year_long)),
  tar_target(ip_age_sex_data, get_ip_age_sex_data(provider_successors_last_updated)),
  tar_target(ip_dsr_data, get_ip_dsr_data(ip_age_sex_data, lkp_peers, catchments, lkp_euro_2013)),
  tar_target(ip_diag_data, get_ip_diag_data(provider_successors_last_updated)),
  tar_target(strategies, get_strategies()),
  tar_target(data_last_updated, {
    withr::with_dir("inst/app/data", {
      save_data(
        age_sex = ip_age_sex_data,
        dsr = ip_dsr_data,
        diagnoses = ip_diag_data
      )

      saveRDS(lkp_peers, "peers.Rds")
      saveRDS(providers, "providers.Rds")
      saveRDS(strategies, "strategies.Rds")
    })

    Sys.time()
  })
)
