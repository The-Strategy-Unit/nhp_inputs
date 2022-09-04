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
mod_mitigators_admission_avoidance_server <- function(id, provider, baseline_year, peers, strategies) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateSelectInput(session, "strategy", choices = strategies)
    })

    ip_age_sex_data <- readRDS(app_sys("app", "data", "ip_age_sex_data.Rds"))
    ip_diag_data <- readRDS(app_sys("app", "data", "ip_diag_data.Rds"))
    ip_dsr_data <- readRDS(app_sys("app", "data", "ip_dsr_data.Rds"))

    output$strategy_text <- shiny::renderText({
      strategy <- shiny::req(input$strategy)

      text_file <- app_sys("app", "strategy_text", paste0(strategy, ".md"))

      if (file.exists(text_file)) {
        # todo: convert to render markdown?
        readr::read_lines(text_file)
      }
    })

    data_path <- shiny::reactive({
      strategy <- req(input$strategy)
      app_sys("app", "data", "providers", provider(), strategy)
    })

    read_data_file <- function(filename) {
      shiny::reactive({
        readRDS(file.path(data_path(), filename))
      })
    }
    dsr_data <- read_data_file("dsr.rds")
    age_sex_data <- read_data_file("age_sex.rds")
    diagnoses_data <- read_data_file("diagnoses.rds")

    trend_data <- shiny::reactive({
      dsr_data() |>
        dplyr::filter(.data$peer == provider())
    })

    output$trend_plot <- shiny::renderPlot({
      dsr_trend_plot(trend_data(), baseline_year())
    })

    funnel_data <- shiny::reactive({
      dsr_data() |>
        dplyr::filter(.data$fyear == baseline_year()) |>
        generate_dsr_funnel_data(provider())
    })

    output$funnel_plot <- shiny::renderPlot({
      plot(funnel_data())
    })
  })
}
