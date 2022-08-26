#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # this will load:
  # * ip_diag_data
  # * ip_dsr_data
  # * lkp_peers
  # * providers
  # * strategies
  ip_diag_data <- readRDS(app_sys("app", "data", "ip_diag_data.Rds"))
  ip_dsr_data <- readRDS(app_sys("app", "data", "ip_dsr_data.Rds"))
  peers <- readRDS(app_sys("app", "data", "peers.Rds"))
  providers <- readRDS(app_sys("app", "data", "providers.Rds"))
  strategies <- readRDS(app_sys("app", "data", "strategies.Rds"))

  selected_provider <- reactive({
    "RL4" # todo: convert to an input
  })

  selected_baseline_year <- reactive({
    201819 # todo: create an input
  })

  mod_mitigators_admission_avoidance_server(
    "mitigators_admission_avoidance",
    selected_provider,
    selected_baseline_year,
    ip_dsr_data,
    ip_diag_data,
    peers,
    strategies[["admission avoidance"]]
  )
}
