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
  all_data <- readRDS(app_sys("app", "data", "provider_data.Rds"))

  home_module <- mod_home_server("home", providers, peers)
  selected_provider <- shiny::reactive(shiny::req(home_module()$provider))
  selected_baseline_year <- shiny::reactive(shiny::req(home_module()$baseline))
  provider_data <- shiny::reactive(all_data[[selected_provider()]])

  mms <- \(id, strats) mod_mitigators_server(
    id,
    selected_provider,
    selected_baseline_year,
    strats,
    provider_data,
    diagnoses_lkup
  )

  params <- list(
    admission_avoidance = list(
      mms(
        "mitigators_admission_avoidance", strategies[["admission avoidance"]]
      )
    ),
    los_reduction = list(
      mean_los = mms(
        "mitigators_mean_los_reduction", strategies[["los reduction"]]
      ),
      aec = mms(
        "mitigators_aec_los_reduction", strategies[["los reduction"]]
      ),
      preop = mms(
        "mitigators_preop_los_reduction", strategies[["los reduction"]]
      ),
      bads = mms(
        "mitigators_bads", strategies[["los reduction"]]
      )
    )
  )

  mod_debug_params_server("debug_params", params)
}
