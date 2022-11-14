#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # load the data
  diagnoses_lkup <- readRDS(app_sys("app", "data", "diagnoses.Rds"))

  home_module <- mod_home_server("home", providers, peers)
  provider <- shiny::reactive(shiny::req(home_module()$provider))
  baseline_year <- shiny::reactive(shiny::req(home_module()$baseline))
  provider_data <- shiny::reactive({
    readRDS(app_sys("app", "data", "provider_data.Rds"))[[provider()]]
  })

  available_strategies <- shiny::reactive({
    s <- readRDS(app_sys("app", "data", "available_strategies.Rds"))

    s[[provider()]][[as.character(baseline_year())]]
  })

  params <- shiny::reactiveValues()

  mod_expat_repat_server("expat_repat", params, provider, baseline_year)

  mms <- \(id, activity_type, mitigators_type) mod_mitigators_server(
    id,
    params,
    activity_type,
    mitigators_type,
    provider,
    baseline_year,
    provider_data,
    available_strategies,
    diagnoses_lkup
  )

  mms("mitigators_admission_avoidance", "inpatient_factors", "admission_avoidance")
  mms("mitigators_mean_los_reduction", "inpatient_factors", "los_reduction|mean_los")
  mms("mitigators_aec_los_reduction", "inpatient_factors", "los_reduction|aec")
  mms("mitigators_preop_los_reduction", "inpatient_factors", "los_reduction|preop")
  mms("mitigators_bads", "inpatient_factors", "los_reducition|bads")
  mms("mitigators_op_c2c_reduction", "outpatient_factors", "consultant_to_consultant_reduction")
  mms("mitigators_op_convert_tele", "outpatient_factors", "convert_to_tele")
  mms("mitigators_op_fup_reduction", "outpatient_factors", "followup_reduction")
  mms("mitigators_aae_frequent_attenders", "aae_factors", "frequent_attenders")
  mms("mitigators_aae_left_before_seen", "aae_factors", "left_before_seen")
  mms("mitigators_aae_low_cost_discharged", "aae_factors", "low_cost_discharged")

  mod_debug_params_server(
    "debug_params",
    params
  )
}
