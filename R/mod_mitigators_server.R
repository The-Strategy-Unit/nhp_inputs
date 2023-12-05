#' mitigators_admission_avoidance Server Functions
#'
#' @noRd
mod_mitigators_server <- function(id, # nolint: object_usage_linter.
                                  params,
                                  provider_data,
                                  available_strategies,
                                  diagnoses_lkup) {
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  config <- get_golem_config("mitigators_config")[[id]]

  activity_type <- config$activity_type
  mitigators_type <- config$mitigators_type

  param_conversion <- list(
    absolute = list(\(r, p) p * r, \(r, q) q / r),
    relative = list(\(r, p) p, \(r, q) q)
  )

  reasons_key <- shiny::reactiveVal()
  mod_reasons_server(
    shiny::NS(id, "reasons"),
    params,
    mitigators_type,
    activity_type,
    key = reasons_key
  )

  shiny::moduleServer(id, function(input, output, session) {
    slider_values <- shiny::reactiveValues()
    output_conversions <- shiny::reactiveValues()
    time_profile_mappings <- shiny::reactiveValues()

    strategies <- shiny::reactive({
      # make sure a provider is selected
      shiny::req(params$dataset)

      shiny::observe(
        input$strategy |>
          shiny::req() |>
          reasons_key()
      ) |>
        shiny::bindEvent(input$strategy)

      # set the names of the strategies to title case, but fix up some of the replaced words to upper case
      config$strategy_subset |>
        intersect(available_strategies()) |>
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

    get_default <- function(rate) {
      c(0.95, 1)
    }

    init <- shiny::observe({
      strategies <- shiny::req(strategies())

      # update the drop down
      shiny::updateSelectInput(session, "strategy", choices = strategies)

      loaded_values <- params |>
        shiny::reactiveValuesToList() |>
        _[c("activity_avoidance", "efficiencies")] |>
        purrr::flatten() |>
        purrr::flatten() |>
        _[strategies] |>
        purrr::map("interval")

      strategies |>
        # remove the friendly name for the strategy, replace with itself
        purrr::set_names() |>
        purrr::walk(\(i) {
          # get the rates data for this strategy (for the provider in the baseline year)
          r <- provider_data()[[i]]$rates |>
            dplyr::filter(
              .data$peer == params$dataset,
              .data$fyear == params$start_year
            )

          slider_values[[mitigators_type]][[i]] <- c(
            # add the additional param items if they exist.
            config$params_items |>
              # if the additional item is a list, chose the value for the current strategy
              purrr::map_if(is.list, ~ .x[[i]]) |>
              # if the additional item is a function, evaluate it with the rates data
              purrr::map_if(is.function, rlang::exec, r),
            list(
              interval = loaded_values[[i]] %||% get_default(r$rate)
            )
          )

          output_conversions[[mitigators_type]][[i]] <- (config$param_output %||% \(...) identity)(r$rate)

          params[[mitigators_type]][[activity_type]][[i]] <- if (!is.null(loaded_values[[i]])) {
            fn <- output_conversions[[mitigators_type]][[i]]

            v <- slider_values[[mitigators_type]][[i]]
            v$interval <- fn(v$interval)

            v
          }
        })

      tpm <- session$userData$params$time_profile_mappings[[mitigators_type]][[activity_type]]
      time_profile_mappings$mappings <- tpm
      params$time_profile_mappings[[mitigators_type]][[activity_type]] <- tpm

      shiny::updateCheckboxInput(
        session,
        "include",
        value = !is.null(params[[mitigators_type]][[activity_type]][[strategies[[1]]]])
      )

      init$destroy()
    })

    # set the strategy text by loading the contents of the file for that strategy
    output$strategy_text <- shiny::renderUI({
      strategy <- shiny::req(input$strategy)

      files <- dir(app_sys("app", "strategy_text"), ".md") |>
        stringr::str_remove("\\.md$")

      md_file_to_html(
        "app", "strategy_text",
        paste0(files[stringr::str_detect(strategy, files)], ".md")
      )
    })

    # load data files ----
    selected_data <- shiny::reactive({
      strategy <- shiny::req(input$strategy)
      provider_data()[[strategy]]
    })
    rates_data <- shiny::reactive({
      d <- shiny::req(selected_data())

      d$rates
    })
    age_sex_data <- shiny::reactive({
      d <- shiny::req(selected_data())
      d$age_sex
    })
    diagnoses_data <- shiny::reactive({
      d <- shiny::req(selected_data())
      d$diagnoses
    })

    # rates data baseline year ----

    rates_baseline_data <- shiny::reactive({
      rates_data() |>
        dplyr::filter(.data$fyear == params$start_year) |>
        dplyr::mutate(is_peer = .data$peer != params$dataset)
    })

    # params controls ----
    get_range <- function(max_value, scale) {
      c(0, max_value, 1) * scale
    }

    provider_max_value <- shiny::reactive({
      r <- dplyr::filter(rates_baseline_data(), !.data$is_peer)$rate
      m <- config$slider_scale / config$slider_step
      floor(r * m) / m
    })

    shiny::observe({
      shiny::req(input$strategy)
      include <- !is.null(params[[mitigators_type]][[activity_type]][[input$strategy]])

      shiny::updateCheckboxInput(session, "include", value = include)
      shiny::updateRadioButtons(session, "slider_type", selected = "% rate")
      update_slider("% change")
    }) |>
      shiny::bindEvent(input$strategy)

    update_slider <- function(type) {
      strategy <- shiny::req(input$strategy)
      max_value <- provider_max_value()

      if (type == "rate") {
        scale <- config$slider_scale
        range <- get_range(max_value, scale)
        step <- config$slider_step
        pc_fn <- param_conversion$absolute[[1]]
      } else {
        scale <- 100
        range <- c(0, 100)
        step <- 0.1
        pc_fn <- param_conversion$relative[[1]]
      }

      values <- pc_fn(max_value, slider_values[[mitigators_type]][[strategy]]$interval) * scale
      shiny::updateSliderInput(
        session, "slider",
        value = values, min = range[[1]], max = range[[2]], step = step
      )

      update_time_profile(
        time_profile_mappings$mappings[[strategy]] %||% "linear"
      )
    }

    shiny::observe({
      shiny::req(input$strategy)
      update_slider(input$slider_type)
    }) |>
      shiny::bindEvent(input$slider_type)

    shiny::observe({
      values <- input$slider
      type <- shiny::req(input$slider_type)
      strategy <- shiny::req(input$strategy)
      max_value <- provider_max_value()

      if (type == "rate") {
        scale <- config$slider_scale
        pc_fn <- param_conversion$absolute[[2]]
      } else {
        scale <- 100
        pc_fn <- param_conversion$relative[[2]]
      }

      v <- pc_fn(max_value, values / scale)

      at <- activity_type
      mt <- mitigators_type
      slider_values[[mt]][[strategy]]$interval <- v


      params[[mt]][[at]][[strategy]] <- if (input$include) {
        fn <- output_conversions[[mitigators_type]][[strategy]]

        v <- slider_values[[mitigators_type]][[strategy]]
        v$interval <- fn(v$interval)

        v
      }
    }) |>
      shiny::bindEvent(input$slider, input$include)

    shiny::observe({
      strategy <- shiny::req(input$strategy)
      tp <- shiny::req(selected_time_profile())

      time_profile_mappings$mappings[[strategy]] <- tp

      params$time_profile_mappings[[mitigators_type]][[activity_type]][[strategy]] <- if (input$include) tp
    }) |>
      shiny::bindEvent(selected_time_profile(), input$include)

    shiny::observe({
      shinyjs::toggleState("slider", condition = input$include)
      shinyjs::toggleState("slider_type", condition = input$include)
    }) |>
      shiny::bindEvent(input$include)

    # plot ribbon to show selected params ----

    plot_ribbon <- shiny::reactive({
      max_value <- provider_max_value()
      values <- param_conversion$absolute[[1]](max_value, slider_values[[mitigators_type]][[input$strategy]]$interval)

      ggplot2::annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = values[[1]],
        ymax = values[[2]],
        colour = "#f9bf07",
        fill = ggplot2::alpha("#f9bf07", 0.2),
        na.rm = TRUE
      )
    })

    # trend plot ----
    # use the rates data, filtered to the provider that has been selected

    trend_data <- shiny::reactive({
      rates_data() |>
        dplyr::filter(.data$peer == params$dataset)
    })

    output$trend_plot <- shiny::renderPlot({
      rates_trend_plot(
        trend_data(),
        params$start_year,
        plot_range(),
        config$y_axis_title,
        config$x_axis_title,
        config$number_type,
        plot_ribbon()
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
      plot(
        funnel_data(),
        plot_range(),
        plot_ribbon(),
        config$funnel_x_title
      )
    })

    # boxplot ----

    output$boxplot <- shiny::renderPlot({
      rates_baseline_data() |>
        tidyr::drop_na(.data$is_peer) |>
        rates_boxplot(plot_range(), plot_ribbon())
    })


    # diagnoses ----

    output$diagnoses_table <- gt::render_gt({
      data <- diagnoses_data() |>
        dplyr::filter(.data$fyear == params$start_year) |>
        dplyr::inner_join(diagnoses_lkup, by = c("diagnosis" = "diagnosis_code")) |>
        dplyr::select("diagnosis_description", "n", "p")

      data <- dplyr::bind_rows(
        data,
        dplyr::summarise(
          data,
          diagnosis_description = "Other",
          p = 1 - sum(.data$p),
          n = sum(.data$n) / (1 - .data$p)
        )
      )

      gt::gt(data, "diagnosis_description") |>
        gt::cols_label(
          "diagnosis_description" = "Diagnosis",
          "n" = "Count of Activity (spells)",
          "p" = "% of Total Activity"
        ) |>
        gt::tab_stubhead("Diagnosis") |>
        gt::fmt_number(
          c("n"),
          decimals = 0,
          use_seps = TRUE
        ) |>
        gt::fmt_percent(
          c("p"),
          decimals = 1
        ) |>
        gt::grand_summary_rows(
          columns = "n",
          fns = list(Total = ~ sum(.)),
          fmt = list(
            ~ gt::fmt_number(., decimals = 0, use_seps = TRUE)
          )
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#EFEFEF"),
            gt::cell_text(weight = "bold")
          ),
          locations = list(
            gt::cells_column_labels(),
            gt::cells_stubhead(),
            gt::cells_grand_summary(),
            gt::cells_stub_grand_summary()
          )
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#FBFBFB"),
            gt::cell_text(weight = "bold")
          ),
          locations = list(
            gt::cells_body(
              rows = .data$diagnosis_description == "Other"
            ),
            gt::cells_stub(
              rows = .data$diagnosis_description == "Other"
            )
          )
        )
    })

    # age group ----

    output$age_grp_plot <- shiny::renderPlot({
      age_data <- age_sex_data() |>
        dplyr::filter(.data$fyear == params$start_year)

      shiny::req(nrow(age_data) > 0)
      age_pyramid(age_data)
    })

    # NEE result

    output$nee_result <- shiny::renderPlot(
      {
        nee_params <- app_sys("app", "data", "nee_table.Rds") |>
          readRDS() |>
          dplyr::filter(.data[["param_name"]] == input$strategy)

        nee_params |>
          ggplot2::ggplot() +
          ggplot2::geom_segment(
            ggplot2::aes(
              y = 1, yend = 1,
              x = .data[["percentile10"]], xend = .data[["percentile90"]]
            ),
            size = 2
          ) +
          ggplot2::geom_point(ggplot2::aes(y = 1, x = mean), size = 5) +
          ggplot2::xlim(0, 100) +
          ggplot2::xlab("80% confidence interval- mean represented as point") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
          ) +
          ggplot2::ggtitle("Nationally determined estimate")
      },
      width = "auto",
      height = 60
    )
  })
}
