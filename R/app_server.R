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
  diagnoses_lkup <- readRDS(app_sys("app", "data", "diagnoses.Rds"))
  all_data <- readRDS(app_sys("app", "data", "provider_data.Rds"))

  home_module <- mod_home_server("home", providers, peers)
  selected_provider <- shiny::reactive(shiny::req(home_module()$provider))
  selected_baseline_year <- shiny::reactive(shiny::req(home_module()$baseline))
  provider_data <- shiny::reactive(all_data[[selected_provider()]])

  expat_repat <- mod_expat_repat_server("expat_repat")

  mms <- \(id) mod_mitigators_server(
    id,
    selected_provider,
    selected_baseline_year,
    provider_data,
    diagnoses_lkup
  )

  activity_mitigators <- list(
    inpatient_factors = list(
      admission_avoidance = list(
        mms("mitigators_admission_avoidance")
      ),
      los_reduction = list(
        mean_los = mms("mitigators_mean_los_reduction"),
        aec = mms("mitigators_aec_los_reduction"),
        preop = mms("mitigators_preop_los_reduction"),
        bads = mms("mitigators_bads")
      )
    ),
    outpatient_factors = list(
      consultant_to_consultant_reduction = list(
        mms("mitigators_op_c2c_reduction")
      ),
      convert_to_tele = list(
        mms("mitigators_op_convert_tele")
      ),
      followup_reduction = list(
        mms("mitigators_op_fup_reduction")
      )
    ),
    aae_factors = list(
      frequent_attenders = list(
        mms("mitigators_aae_frequent_attenders")
      ),
      left_before_seen = list(
        mms("mitigators_aae_left_before_seen")
      ),
      low_cost_discharged = list(
        mms("mitigators_aae_low_cost_discharged")
      )
    )
  )

  mod_debug_params_server(
    "debug_params",
    expat_repat,
    activity_mitigators
  )
}
