#' mitigators_admission_avoidance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mitigators_admission_avoidance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("strategy"), "Strategy", choices = NULL),
    shinycssloaders::withSpinner({
      shiny::plotOutput(ns("funnel_plot"))
    })
  )
}

#' mitigators_admission_avoidance Server Functions
#'
#' @noRd
mod_mitigators_admission_avoidance_server <- function(id, provider, baseline_year, dsr_data, diag_data, peers,
                                                      strategies) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateSelectInput(session, "strategy", choices = strategies)
    })

    funnel_data <- shiny::reactive({
      calculate_funnel_plot_data(dsr_data, peers, provider(), baseline_year(), input$strategy)
    })

    output$funnel_plot <- shiny::renderPlot({
      plot(funnel_data())
    })
  })
}
