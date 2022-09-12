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

  selected_provider <- shiny::reactive({
    "RL4" # todo: convert to an input
  })

  selected_baseline_year <- shiny::reactive({
    201819 # todo: create an input
  })

  mod_mitigators_admission_avoidance_server(
    "mitigators_admission_avoidance",
    selected_provider,
    selected_baseline_year,
    strategies[["admission avoidance"]]
  )
}
