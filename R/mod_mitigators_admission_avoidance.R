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
    shiny::uiOutput(ns("strategy_text")),
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

    # set the strategy text by loading the contents of the file for that strategy
    output$strategy_text <- shiny::renderUI({
      strategy <- shiny::req(input$strategy)
      file <- app_sys("app", "strategy_text", paste0(strategy, ".md"))
      req(file.exists(file))
      shiny::htmlTemplate(text_ = markdown::renderMarkdown(file))
    })

    # load data files ----
    # create a reactive for the path to where our data files live
    data_path <- shiny::reactive({
      strategy <- req(input$strategy)
      app_sys("app", "data", "providers", provider(), strategy)
    })
    # a helper function to create a reactive to load a specific file
    read_data_file <- function(filename) {
      shiny::reactive(readRDS(file.path(data_path(), filename)))
    }
    # create the reactives to load the files
    dsr_data <- read_data_file("dsr.rds")
    age_sex_data <- read_data_file("age_sex.rds")
    diagnoses_data <- read_data_file("diagnoses.rds")

    # trend plot ----
    # use the DSR data, filtered to the provider that has been selected
    trend_data <- shiny::reactive({
      dsr_data() |>
        dplyr::filter(.data$peer == provider())
    })

    output$trend_plot <- shiny::renderPlot({
      dsr_trend_plot(trend_data(), baseline_year())
    })

    # funnel plot ----
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
