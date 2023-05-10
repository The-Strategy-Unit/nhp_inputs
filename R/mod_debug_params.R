#' debug_params UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_debug_params_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("DEBUG: selected parameter values"),
    shiny::verbatimTextOutput(ns("params_json"))
  )
}

#' debug_params Server Functions
#'
#' @noRd
mod_debug_params_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    fixed_params <- shiny::reactive({
      p <- shiny::reactiveValuesToList(params)

      # combine efficiences/activity avoidance items
      p[["efficiencies"]] <- list(
        ip = c(
          p[["efficiencies|mean_los"]]$ip,
          p[["efficiencies|aec"]]$ip,
          p[["efficiencies|preop"]]$ip,
          p[["efficiencies|bads"]]$ip
        ),
        op = c(
          p[["efficiencies|ctt"]]$op
        )
      )

      p[["activity_avoidance"]]$op <- c(
        p[["activity_avoidance|c2c"]]$op,
        p[["activity_avoidance|f2f"]]$op
      )

      p[["activity_avoidance"]]$aae <- c(
        p[["activity_avoidance|fa"]]$aae,
        p[["activity_avoidance|lbs"]]$aae,
        p[["activity_avoidance|lcd"]]$aae
      )

      # remove the items
      p[["efficiencies|mean_los"]] <- NULL
      p[["efficiencies|aec"]] <- NULL
      p[["efficiencies|preop"]] <- NULL
      p[["efficiencies|bads"]] <- NULL
      p[["efficiencies|ctt"]] <- NULL
      p[["activity_avoidance|c2c"]] <- NULL
      p[["activity_avoidance|f2f"]] <- NULL
      p[["activity_avoidance|fa"]] <- NULL
      p[["activity_avoidance|lbs"]] <- NULL
      p[["activity_avoidance|lcd"]] <- NULL

      # convert financial year to calendar year
      p$start_year <- as.integer(stringr::str_sub(p$start_year, 1, 4))

      p
    })

    output$params_json <- shiny::renderPrint({
      p <- fixed_params()

      p_order <- c(
        "scenario",
        "dataset",
        "seed",
        "model_runs",
        "start_year",
        "end_year",
        "app_version",
        "create_datetime",
        "demographic_factors",
        "health_status_adjustment",
        "life_expectancy",
        "expat",
        "repat_local",
        "repat_nonlocal",
        "baseline_adjustment",
        "waiting_list_adjustment",
        "non-demographic_adjustment",
        "activity_avoidance",
        "efficiencies",
        "bed_occupancy",
        "theatres"
      )

      p_order <- p_order[p_order %in% names(p)]

      p[c(p_order, setdiff(names(p), p_order))] |>
        jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
    })
  })
}
