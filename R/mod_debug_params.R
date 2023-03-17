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
    shiny::selectInput(
      ns("param_item"),
      "parameter",
      choices = NULL
      #   c(
      #     "demographic_factors",
      #     "health_status_adjustment",
      #     "life_expectancy",
      #     "expat",
      #     "repat_local",
      #     "repat_nonlocal",
      #     "baseline_adjustment",
      #     "waiting_list_adjustment",
      #     "non-demographic_adjustment",
      #     "activity_avoidance",
      #     "efficiencies",
      #     "bed_occupancy",
      #     "theatres"
      #   )
    ),
    shiny::verbatimTextOutput(ns("params_json"))
  )
}

#' debug_params Server Functions
#'
#' @noRd
mod_debug_params_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      p <- fixed_params()
      shiny::updateSelectInput(session, "param_item", choices = names(p))
    })

    fixed_params <- shiny::reactive({
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

      # fix the values
      p$activity_avoidance <- purrr::map_depth(p$activity_avoidance, 2, run_param_output)
      p$efficiencies <- purrr::map_depth(p$efficiencies, 2, run_param_output)

      p
    })

    output$params_json <- shiny::renderPrint({
      i <- shiny::req(input$param_item)
      p <- fixed_params()[[i]]

      jsonlite::toJSON(p, pretty = TRUE, auto_unbox = TRUE)
    })
  })
}
