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
    shiny::textOutput(ns("strategy_text")),
    shinycssloaders::withSpinner({
      shiny::plotOutput(ns("trend_plot"))
    }),
    shinycssloaders::withSpinner({
      shiny::plotOutput(ns("funnel_plot"))
    })
  )
}

dsr_trend_plot <- function(trend_data, baseline_year) {
  ggplot2::ggplot(trend_data, ggplot2::aes(.data$fyear, .data$std_rate)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(colour = .data$fyear == baseline_year)) +
    ggplot2::theme(legend.position = "none")
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

    output$strategy_text <- shiny::renderText({
      strategy <- shiny::req(input$strategy)

      text_file <- app_sys("app", "strategy_text", paste0(strategy, ".md"))

      if (file.exists(text_file)) {
        # todo: convert to render markdown?
        readr::read_lines(text_file)
      }
    })

    filtered_dsr_data <- shiny::reactive({
      dsr_data |>
        dplyr::filter(.data$procode == provider(), .data$strategy == input$strategy)
    })

    trend_data <- shiny::reactive({
      filtered_dsr_data() |>
        dplyr::filter(.data$peer == provider())
    })

    output$trend_plot <- shiny::renderPlot({
      dsr_trend_plot(trend_data(), baseline_year())
    })

    funnel_data <- shiny::reactive({
      filtered_dsr_data() |>
        dplyr::filter(.data$fyear == baseline_year()) |>
        generate_dsr_funnel_data(provider())
    })

    output$funnel_plot <- shiny::renderPlot({
      plot(funnel_data())
    })
  })
}
