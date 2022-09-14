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

  home_module <- mod_home_server("home", providers, peers)
  selected_provider <- shiny::reactive(shiny::req(home_module()$provider))
  selected_baseline_year <- shiny::reactive(shiny::req(home_module()$baseline))

  mod_mitigators_admission_avoidance_server(
    "mitigators_admission_avoidance",
    selected_provider,
    selected_baseline_year,
    strategies[["admission avoidance"]]
  )
}
