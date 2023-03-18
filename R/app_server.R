#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # load the data
  diagnoses_lkup <- readRDS(app_sys("app", "data", "diagnoses.Rds"))
  providers <- readRDS(app_sys("app", "data", "providers.Rds"))

  params <- shiny::reactiveValues()
  params[["demographic_factors"]] <- list(
    file = "demographic_factors.csv"
  )

  mod_home_server("home", providers, params)

  provider <- shiny::reactive(shiny::req(params$dataset))
  baseline_year <- shiny::reactive(shiny::req(params$start_year))
  provider_data <- shiny::reactive({
    readRDS(app_sys("app", "data", "provider_data.Rds"))[[provider()]]
  })

  available_strategies <- shiny::reactive({
    s <- readRDS(app_sys("app", "data", "available_strategies.Rds"))

    s[[provider()]][[as.character(baseline_year())]]
  })

  mod_expat_repat_server("expat_repat", params, provider, baseline_year, providers)

  mod_population_growth_server("population_growth", params)
  mod_hsa_server("hsa", params)
  mod_nda_server("nda", params)
  mod_wli_server("wli", params)

  purrr::walk(
    c(
      "mitigators_admission_avoidance",
      "mitigators_mean_los_reduction",
      "mitigators_aec_los_reduction",
      "mitigators_preop_los_reduction",
      "mitigators_bads",
      "mitigators_op_c2c_reduction",
      "mitigators_op_convert_tele",
      "mitigators_op_fup_reduction",
      "mitigators_aae_frequent_attenders",
      "mitigators_aae_left_before_seen",
      "mitigators_aae_low_cost_discharged"
    ),
    mod_mitigators_server,
    params,
    provider,
    baseline_year,
    provider_data,
    available_strategies,
    diagnoses_lkup
  )

  mod_debug_params_server(
    "debug_params",
    params
  )
}
