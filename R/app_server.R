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

  provider_data <- shiny::reactive({
    provider <- shiny::req(params$dataset)
    file <- glue::glue("{provider}/data.rds")

    load_rds_from_adls(file)
  })

  available_strategies <- shiny::reactive({
    provider <- shiny::req(params$dataset)
    year <- as.character(shiny::req(params$start_year))

    load_rds_from_adls(glue::glue("{provider}/available_strategies.rds"))[[year]]
  })

  mod_expat_repat_server("expat_repat", params, providers)

  mod_population_growth_server("population_growth", params)
  mod_hsa_server("hsa", params)

  mod_nda_server("nda", params)
  mod_wli_server("wli", params)

  mod_theatres_server("theatres", params)
  mod_bed_occupancy_server("bed_occupancy", params)

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
    provider_data,
    available_strategies,
    diagnoses_lkup
  )

  mod_debug_params_server(
    "debug_params",
    params
  )

  if (!getOption("golem.app.prod", FALSE)) {
    cat("auto reconnect enabled\n")
    session$allowReconnect("force")
  }
}
