#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # load the data
  peers <- readRDS(app_sys("app", "data", "peers.Rds"))
  providers <- readRDS(app_sys("app", "data", "providers.Rds"))
  strategies <- readRDS(app_sys("app", "data", "strategies.Rds"))
  diagnoses_lkup <- readRDS(app_sys("app", "data", "diagnoses.Rds"))

  home_module <- mod_home_server("home", providers, peers)
  selected_provider <- shiny::reactive(shiny::req(home_module()$provider))
  selected_baseline_year <- shiny::reactive(shiny::req(home_module()$baseline))

  mms <- \(id, strats) mod_mitigators_server(
    id,
    selected_provider,
    selected_baseline_year,
    strats,
    diagnoses_lkup
  )

  mms("mitigators_admission_avoidance", strategies[["admission avoidance"]])

  mms("mitigators_mean_los_reduction", c(
    "emergency_elderly",
    "enhanced_recovery_bladder",
    "enhanced_recovery_breast",
    "enhanced_recovery_colectomy",
    "enhanced_recovery_hip",
    "enhanced_recovery_hysterectomy",
    "enhanced_recovery_knee",
    "enhanced_recovery_prostate",
    "enhanced_recovery_rectum",
    "excess_beddays_elective",
    "excess_beddays_emergency",
    "raid_ip",
    "stroke_early_supported_discharge"
  ))

  mms("mitigators_aec_los_reduction", c(
    "ambulatory_emergency_care_low",
    "ambulatory_emergency_care_moderate",
    "ambulatory_emergency_care_high",
    "ambulatory_emergency_care_very_high"
  ))

  mms("mitigators_preop_los_reduction", c(
    "pre-op_los_1-day",
    "pre-op_los_2-day"
  ))

  mms("mitigators_bads", c(
    "bads_daycase_occasional",
    "bads_daycase",
    "bads_outpatients",
    "bads_outpatients_or_daycase"
  ))
}
