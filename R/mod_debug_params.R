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
    output$params_json <- shiny::renderPrint({
      p <- shiny::reactiveValuesToList(params)

      run_param_output <- function(.x) {
        if (is.null(.x)) {
          return(.x)
        }
        .x$interval <- .x$param_output(.x$rate, .x$interval)
        .x$param_output <- NULL
        .x$rate <- NULL
        .x
      }

      # the activity mitigators all need to have their data cleaned up ready for use by the model
      p$inpatient_factors <- purrr::map_depth(p$inpatient_factors, 2, run_param_output)
      p$outpatient_factors <- purrr::map_depth(p$outpatient_factors, 2, run_param_output)
      p$aae_factors <- purrr::map_depth(p$aae_factors, 2, run_param_output)

      # combine results together
      p$inpatient_factors$los_reduction <- purrr::flatten(
        p$inpatient_factors[c(
          "los_reduction|mean_los",
          "los_reduction|aec",
          "los_reduction|preop",
          "los_reduction|bads"
        )]
      )
      p$inpatient_factors[c(
        "los_reduction|mean_los",
        "los_reduction|aec",
        "los_reduction|preop",
        "los_reduction|bads"
      )] <- NULL

      jsonlite::toJSON(p, pretty = TRUE, auto_unbox = TRUE)
    })
  })
}
