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
  config <- get_golem_config("mitigators_config")[[id]]

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Activity Mitigators"),
    shiny::h2(title),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::fluidRow(
          bs4Dash::box(
            title = "Activity Mitigator",
            width = 12,
            shiny::selectInput(ns("strategy"), "Selection", choices = NULL),
            shiny::uiOutput(ns("strategy_text"))
          ),
          bs4Dash::box(
            title = "Params",
            width = 12,
            shiny::radioButtons(
              ns("slider_type"),
              "Slider Type",
              c("rate", "% change"),
              "rate"
            ),
            shiny::sliderInput(ns("slider"), "Slider", 0, 1, c(0, 1))
          )
        )
      ),
      shiny::column(
        width = 9,
        shiny::fluidRow(
          bs4Dash::box(
            title = config$trend_box_title,
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("trend_plot"))
            }),
            width = 5
          ),
          bs4Dash::box(
            title = config$funnel_box_title,
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("funnel_plot"))
            }),
            width = 5
          ),
          bs4Dash::box(
            title = config$boxplot_title,
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("boxplot"))
            }),
            width = 2
          ),
          bs4Dash::box(
            title = "Top 6 Primary Diagnoses",
            shinycssloaders::withSpinner({
              gt::gt_output(ns("diagnoses_table"))
            }),
            width = 6
          ),
          bs4Dash::box(
            title = "Bar Chart of Activity by Age and Sex",
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

#' mitigators_admission_avoidance Server Functions
#'
#' @noRd
mod_mitigators_server <- function(id, provider, baseline_year, provider_data, diagnoses_lkup) {
  shiny::moduleServer(id, function(input, output, session) {
    config <- get_golem_config("mitigators_config")[[id]]

    params <- purrr::lift_dl(shiny::reactiveValues)(
      config$strategy_subset |>
        purrr::set_names() |>
        purrr::map(~NULL)
    )

    strategies <- shiny::reactive({
      # make sure a provider is selected
      shiny::req(provider())

      # find the strategies that are available for this provider
      available_strategies <- names(provider_data())
      # set the names of the strategies to title case, but fix up some of the replaced words to upper case
      config$strategy_subset |>
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
    })

    shiny::observe({
      # update the drop down
      shiny::updateSelectInput(session, "strategy", choices = strategies())

      # reset the params reactiveValues
      for (i in names(params)) {
        # if the strategy `i` is not valid for this provider, then params[[i]] == NULL
        params[[i]] <- if (i %in% strategies()) {
          # get the rates data for this strategy (for the provider in the baseline year)
          r <- provider_data()[[i]]$rates |>
            dplyr::filter(
              .data$peer == provider(),
              .data$fyear == baseline_year()
            )

          c(
            # add the additional param items if they exist.
            # if the additional item is a function, evaluate it with the rates data
            purrr::map_if(config$params_items, is.function, rlang::exec, r),
            list(interval = c(0.95, 1))
          )
        }
      }
    })

    # set the strategy text by loading the contents of the file for that strategy
    output$strategy_text <- shiny::renderUI({
      strategy <- shiny::req(input$strategy)

      files <- dir(app_sys("app", "strategy_text"), ".md") |>
        stringr::str_remove("\\.md$")

      file <- app_sys(
        "app", "strategy_text",
        paste0(files[stringr::str_detect(strategy, files)], ".md")
      )

      shiny::req(file.exists(file))
      shiny::htmlTemplate(text_ = markdown::renderMarkdown(file))
    })

    # load data files ----
    selected_data <- shiny::reactive({
      strategy <- shiny::req(input$strategy)
      provider_data()[[strategy]]
    })
    rates_data <- shiny::reactive(selected_data()$rates)
    age_sex_data <- shiny::reactive(selected_data()$age_sex)
    diagnoses_data <- shiny::reactive(selected_data()$diagnoses)

    # rates data baseline year ----

    rates_baseline_data <- shiny::reactive({
      rates_data() |>
        dplyr::filter(.data$fyear == baseline_year()) |>
        dplyr::mutate(is_peer = .data$peer != .env$provider())
    })

    # params controls ----

    provider_max_value <- shiny::reactive({
      r <- dplyr::filter(rates_baseline_data(), !.data$is_peer)$rate
      m <- config$slider_scale / config$slider_step
      floor(r * m) / m
    })

    shiny::observe({
      shiny::updateRadioButtons(session, "slider_type", selected = "rate")
      update_slider("rate")
    }) |>
      shiny::bindEvent(input$strategy)

    update_slider <- function(type) {
      strategy <- shiny::req(input$strategy)
      values <- params[[strategy]]$interval
      max_value <- provider_max_value()

      if (type == "rate") {
        new_max <- max_value * config$slider_scale
        values <- values * max_value * config$slider_scale
        step <- config$slider_step
      } else {
        new_max <- 100
        values <- values * 100
        step <- 0.1
      }

      shiny::updateSliderInput(session, "slider", value = values, min = 0, max = new_max, step = step)
    }

    shiny::observe({
      update_slider(input$slider_type)
    }) |>
      shiny::bindEvent(input$slider_type)

    shiny::observe({
      values <- input$slider
      type <- shiny::req(input$slider_type)
      strategy <- shiny::req(input$strategy)
      max_value <- provider_max_value()

      if (type == "rate") {
        values <- values / (max_value * config$slider_scale)
      } else {
        values <- values / 100
      }
      params[[strategy]]$interval <- values
    }) |>
      shiny::bindEvent(input$slider)

    # trend plot ----
    # use the rates data, filtered to the provider that has been selected

    trend_data <- shiny::reactive({
      rates_data() |>
        dplyr::filter(.data$peer == provider())
    })

    output$trend_plot <- shiny::renderPlot({
      rates_trend_plot(
        trend_data(),
        baseline_year(),
        plot_range(),
        config$y_axis_title,
        config$x_axis_title,
        config$number_type
      )
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
        funnel_data()$rate,
        funnel_data()$lower3,
        funnel_data()$upper3
      )) |>
        pmax(0)
    })

    output$funnel_plot <- shiny::renderPlot({
      plot(funnel_data(), plot_range(), config$funnel_x_title)
    })

    # boxplot ----

    output$boxplot <- shiny::renderPlot({
      rates_baseline_data() |>
        tidyr::drop_na(.data$is_peer) |>
        rates_boxplot(plot_range())
    })


    # diagnoses ----

    output$diagnoses_table <- gt::render_gt({
      data <- diagnoses_data() |>
        dplyr::filter(.data$fyear == baseline_year()) |>
        dplyr::inner_join(diagnoses_lkup, by = c("diagnosis" = "diagnosis_code")) |>
        dplyr::select("diagnosis_description", "n", "p")

      data <- dplyr::bind_rows(
        data,
        dplyr::summarise(
          data,
          diagnosis_description = "Other",
          p = 1 - sum(.data$p),
          n = sum(.data$n) * .data$p
        )
      )

      gt::gt(data) |>
        gt::cols_label(
          "diagnosis_description" = "Diagnosis",
          "n" = "Count of Activity (spells)",
          "p" = "% of Total Activity"
        ) |>
        gt::fmt_number(
          c("n"),
          decimals = 0,
          use_seps = TRUE
        ) |>
        gt::fmt_percent(
          c("p"),
          decimals = 1
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#EFEFEF"),
            gt::cell_text(weight = "bold")
          ),
          locations = list(
            gt::cells_column_labels(),
            gt::cells_body(
              rows = .data$diagnosis_description == "Other"
            )
          )
        )
    })

    # age group ----

    output$age_grp_plot <- shiny::renderPlot({
      age_sex_data() |>
        dplyr::filter(.data$fyear == baseline_year()) |>
        dplyr::count(.data$sex, .data$age_group, wt = .data$n) |>
        age_pyramid()
    })

    # return ----

    params
  })
}
