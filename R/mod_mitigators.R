#' mitigators_admission_avoidance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mitigators_ui <- function(id, title) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Activity Mitigators"),
    shiny::h2(title),
    shiny::fluidRow(
      bs4Dash::box(
        title = "Strategy Selection",
        width = 3,
        shiny::selectInput(ns("strategy"), "Strategy", choices = NULL),
        shiny::uiOutput(ns("strategy_text"))
      ),
      shiny::column(
        width = 9,
        shiny::fluidRow(
          bs4Dash::box(
            title = "Trend",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("trend_plot"))
            }),
            width = 5
          ),
          bs4Dash::box(
            title = "Funnel",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("funnel_plot"))
            }),
            width = 5
          ),
          bs4Dash::box(
            title = "Boxplot",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("boxplot"))
            }),
            width = 2
          ),
          bs4Dash::box(
            title = "Top 6 Diagnoses",
            shinycssloaders::withSpinner({
              shiny::tableOutput(ns("diagnoses_table"))
            }),
            width = 6
          ),
          bs4Dash::box(
            title = "Age and Sex",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("age_grp_plot"))
            }),
            width = 6
          )
        )
      )
    )
  )
}

rates_trend_plot <- function(trend_data, baseline_year, plot_range) {
  ggplot2::ggplot(trend_data, ggplot2::aes(.data$fyear, .data$rate)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = \(.x) dplyr::filter(.x, .data$fyear == baseline_year),
      colour = "red"
    ) +
    ggplot2::scale_y_continuous(limits = plot_range) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank()
    )
}

rates_boxplot <- function(trend_data, plot_range) {
  ggplot2::ggplot(trend_data, ggplot2::aes(x = "", y = .data$rate)) +
    ggplot2::geom_boxplot(alpha = 0.2, outlier.shape = NA) +
    ggbeeswarm::geom_quasirandom(ggplot2::aes(colour = .data$is_peer)) +
    ggplot2::scale_y_continuous(limits = plot_range) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "red")) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_blank()
    )
}

age_pyramid <- function(age_data) {
  age_data |>
    ggplot2::ggplot(ggplot2::aes(.data$n, .data$age_group)) +
    ggplot2::geom_col() +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank()
    )
}

#' mitigators_admission_avoidance Server Functions
#'
#' @noRd
mod_mitigators_server <- function(id, provider, baseline_year, strategies, diagnoses_lkup) {
  shiny::moduleServer(id, function(input, output, session) {
    # on load, update the strategy drop down to include the strategies that are available
    shiny::observe({
      # find the strategies that are available for this provider
      p <- shiny::req(provider())
      available_strategies <- dir(app_sys("app", "data", "providers", p))
      # set the names of the strategies to title case, but fix up some of the replaced words to upper case
      strategies <- strategies |>
        intersect(available_strategies) |>
        purrr::set_names(
          purrr::compose(
            purrr::partial(
              gsub,
              pattern = "(Bads |Eol |Ent$|Gi |Msk$|Nsaids$|Los |Ae$|Ip$)",
              replacement = "\\U\\1",
              perl = TRUE
            ),
            snakecase::to_title_case
          )
        )
      # update the drop down
      shiny::updateSelectInput(session, "strategy", choices = strategies)
    })

    # set the strategy text by loading the contents of the file for that strategy
    output$strategy_text <- shiny::renderUI({
      strategy <- shiny::req(input$strategy)
      file <- app_sys("app", "strategy_text", paste0(strategy, ".md"))
      shiny::req(file.exists(file))
      shiny::htmlTemplate(text_ = markdown::renderMarkdown(file))
    })

    # load data files ----
    # create a reactive for the path to where our data files live
    data_path <- shiny::reactive({
      strategy <- shiny::req(input$strategy)
      app_sys("app", "data", "providers", provider(), strategy)
    })
    # a helper function to create a reactive to load a specific file
    read_data_file <- function(filename) {
      shiny::reactive(readRDS(file.path(data_path(), filename)))
    }
    # create the reactives to load the files
    rates_data <- read_data_file("rates.rds")
    age_sex_data <- read_data_file("age_sex.rds")
    diagnoses_data <- read_data_file("diagnoses.rds")

    # rates data baseline year ----

    rates_baseline_data <- shiny::reactive({
      rates_data() |>
        dplyr::filter(.data$fyear == baseline_year()) |>
        dplyr::mutate(is_peer = .data$peer != .env$provider())
    })

    # trend plot ----
    # use the rates data, filtered to the provider that has been selected

    trend_data <- shiny::reactive({
      rates_data() |>
        dplyr::filter(.data$peer == provider())
    })

    output$trend_plot <- shiny::renderPlot({
      rates_trend_plot(trend_data(), baseline_year(), plot_range())
    })

    # funnel plot ----
    funnel_data <- shiny::reactive({
      rates_baseline_data() |>
        generate_rates_funnel_data()
    })


    # calculate thge range across our plots
    plot_range <- shiny::reactive({
      range(c(
        trend_data()$rate,
        funnel_data()$lower3,
        funnel_data()$upper3
      ))
    })

    output$funnel_plot <- shiny::renderPlot({
      plot(funnel_data(), plot_range())
    })

    # boxplot ----

    output$boxplot <- shiny::renderPlot({
      rates_baseline_data() |>
        tidyr::drop_na(.data$is_peer) |>
        rates_boxplot(plot_range())
    })


    # diagnoses ----

    output$diagnoses_table <- shiny::renderTable({
      diagnoses_data() |>
        dplyr::filter(fyear == baseline_year()) |>
        dplyr::left_join(diagnoses_lkup, by = c(diagnosis = "diagnosis_code")) |>
        dplyr::mutate(`%` = scales::percent(p, accuracy = 1)) |>
        dplyr::select(
          "Diagnosis Description" = .data$diagnosis_description,
          Activity = .data$n,
          `%`
        )
    })

    # age group ----

    output$age_grp_plot <- shiny::renderPlot({
      age_sex_data() |>
        dplyr::filter(.data$fyear == baseline_year()) |>
        dplyr::count(age_group, wt = n) |>
        tidyr::complete(age_group, fill = list(n = 0)) |>
        age_pyramid()
    })
  })
}
