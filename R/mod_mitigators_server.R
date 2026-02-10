#' mitigators_admission_avoidance Server Functions
#'
#' @noRd
mod_mitigators_server <- function(
  id, # nolint: object_usage_linter.
  params,
  rates_data,
  age_sex_data,
  diagnoses_data,
  procedures_data,
  available_strategies,
  diagnoses_lkup,
  procedures_lkup,
  mitigator_codes_lkup,
  peers
) {
  selected_time_profile <- update_time_profile <- NULL
  # nolint start: object_usage_linter
  c(selected_time_profile, update_time_profile) %<-%
    mod_time_profile_server(
      shiny::NS(id, "time_profile"),
      params
    )
  # nolint end

  config <- get_golem_config("mitigators_config")[[id]]

  activity_type <- config$activity_type
  mitigators_type <- config$mitigators_type

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

      # need to invert this list (flip names -> values)
      strats_subset <- config$strategy_subset
      available_subset <- intersect(
        names(strats_subset),
        available_strategies()
      )

      purrr::set_names(
        available_subset,
        mitigator_codes_lkup[available_subset] # e.g. 'IP-EF-017: Enhanced Recovery (Hip)'
      )
    })

    get_default <- function(rate) {
      c(0.95, 1)
    }

    init <- shiny::observe(
      {
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
            r <- rates_data |>
              dplyr::filter(
                .data$strategy == strategies[i],
                .data$provider == params$dataset,
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

            params[[mitigators_type]][[activity_type]][[i]] <- if (
              !is.null(loaded_values[[i]])
            ) {
              slider_values[[mitigators_type]][[i]]
            }
          })

        tpm <- params$time_profile_mappings[[mitigators_type]][[activity_type]]
        time_profile_mappings$mappings <- tpm

        init$destroy()
      },
      priority = 100
    )

    # set the strategy text by loading the contents of the file for that strategy
    output$strategy_text <- shiny::renderUI({
      strategy <- shiny::req(input$strategy)

      files <- dir(app_sys("app", "strategy_text"), ".md") |>
        stringr::str_remove("\\.md$")

      md_file_to_html(
        "app",
        "strategy_text",
        paste0(files[stringr::str_detect(strategy, files)], ".md")
      )
    })

    # rates data baseline year ----

    rates_baseline_data <- shiny::reactive({
      strategy <- shiny::req(input$strategy)

      # nolint start: object_usage_linter
      scheme_peers <- peers |>
        dplyr::filter(
          .data$procode == params$dataset & .data$peer != params$dataset
        ) |>
        dplyr::pull(.data$peer)
      # nolint end

      rates_data |>
        dplyr::filter(
          .data$strategy == .env$strategy,
          .data$fyear == params$start_year
        ) |>
        dplyr::mutate(
          is_peer = dplyr::case_when(
            .data$provider == params$dataset ~ FALSE,
            .data$provider %in% .env$scheme_peers ~ TRUE,
            .default = NA # if scheme is neither focal nor a peer
          )
        ) |>
        dplyr::arrange(dplyr::desc(.data$is_peer)) # to plot focal scheme last
    })

    # params controls ----

    provider_max_value <- shiny::reactive({
      r <- dplyr::filter(rates_baseline_data(), !.data$is_peer)$rate
      m <- config$slider_scale / config$slider_step
      floor(r * m) / m
    })

    shiny::observe({
      # ensure include checkbox is on or off given param value
      strategy <- shiny::req(input$strategy)
      # Wait for slider to be initialized before updating checkbox
      shiny::req(slider_values[[mitigators_type]][[strategy]])
      include <- !is.null(params[[mitigators_type]][[activity_type]][[
        strategy
      ]])
      shiny::updateCheckboxInput(session, "include", value = include)
    }) |>
      shiny::bindEvent(input$strategy)

    shiny::observe({
      # update slider
      strategy <- shiny::req(input$strategy)
      max_value <- provider_max_value()
      scale <- 100

      values <- slider_values[[mitigators_type]][[strategy]]$interval * scale
      shiny::updateSliderInput(
        session,
        "slider",
        value = values
      )

      update_time_profile(
        time_profile_mappings$mappings[[strategy]] %||% "linear"
      )
    }) |>
      shiny::bindEvent(input$strategy)

    shiny::observe({
      values <- input$slider
      strategy <- shiny::req(input$strategy)
      # Ensure slider values have been initialized for this strategy
      shiny::req(slider_values[[mitigators_type]][[strategy]])
      max_value <- provider_max_value()
      scale <- 100

      at <- activity_type
      mt <- mitigators_type
      slider_values[[mt]][[strategy]]$interval <- values / scale

      params[[mt]][[at]][[strategy]] <- if (input$include) {
        slider_values[[mitigators_type]][[strategy]]
      }
    }) |>
      shiny::bindEvent(input$slider, input$include)

    shiny::observe({
      strategy <- shiny::req(input$strategy)
      tp <- shiny::req(selected_time_profile())

      time_profile_mappings$mappings[[strategy]] <- tp

      params$time_profile_mappings[[mitigators_type]][[activity_type]][[
        strategy
      ]] <- if (input$include) tp
    }) |>
      shiny::bindEvent(selected_time_profile(), input$include)

    shiny::observe({
      shinyjs::toggleState("slider", condition = input$include)
    }) |>
      shiny::bindEvent(input$include)

    # plot ribbon to show selected params ----

    plot_ribbon <- shiny::reactive({
      baseline_value <- dplyr::filter(
        rates_baseline_data(),
        !.data$is_peer
      )$rate

      # convert the slider values to absolute values
      interval <- slider_values[[mitigators_type]][[input$strategy]]$interval
      values <- interval * baseline_value

      colour <- "#f9bf07"

      if (input$include) {
        ggplot2::annotate(
          "rect",
          xmin = -Inf,
          xmax = Inf,
          ymin = values[[1]],
          ymax = values[[2]],
          colour = colour,
          fill = ggplot2::alpha(colour, 0.2),
          na.rm = TRUE
        )
      }
    })

    # trend plot ----
    # use the rates data, filtered to the provider that has been selected

    trend_data <- shiny::reactive({
      strategy <- shiny::req(input$strategy)
      rates_data |>
        dplyr::filter(
          .data$strategy == .env$strategy,
          .data$provider == params$dataset
        )
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

    # calculate the range across our plots
    plot_range <- shiny::reactive({
      td_rate <- shiny::req(trend_data())$rate

      fd <- shiny::req(funnel_data())
      fd$z <- attr(fd, "funnel_calculations")$z

      fd_rate <- fd |>
        dplyr::filter(
          .data$denominator >= max(.data$denominator) * 0.05,
          abs(.data$z) < 4
        ) |>
        dplyr::pull("rate")

      c(0, max(c(td_rate, fd_rate)))
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
        rates_boxplot(plot_range(), plot_ribbon())
    })

    # diagnoses ----

    output$diagnoses_table <- gt::render_gt({
      shiny::validate(
        shiny::need(
          diagnoses_data,
          message = "Insufficient or suppressed data."
        )
      )

      strategy <- shiny::req(input$strategy)

      data <- diagnoses_data |>
        dplyr::filter(
          .data$provider == params$dataset,
          .data$strategy == .env$strategy,
          .data$fyear == params$start_year
        ) |>
        dplyr::inner_join(
          diagnoses_lkup,
          by = c("diagnosis" = "diagnosis_code")
        ) |>
        dplyr::select("diagnosis_description", "n", "pcnt")

      n_total <- sum(data$n)
      pcnt_total <- sum(data$pcnt)

      # if we need to include an other row
      if (pcnt_total < 1) {
        data <- dplyr::bind_rows(
          data,
          tibble::tibble(
            diagnosis_description = "Other",
            n = n_total * (1 - pcnt_total) / pcnt_total,
            pcnt = 1 - pcnt_total
          )
        )
      }

      gt::gt(data, "diagnosis_description") |>
        gt::cols_label(
          "diagnosis_description" = "Diagnosis",
          "n" = "Count of Activity (spells)",
          "pcnt" = "% of Total Activity"
        ) |>
        gt::tab_stubhead("Diagnosis") |>
        gt::fmt_number(
          c("n"),
          decimals = 0,
          use_seps = TRUE
        ) |>
        gt::fmt_percent(
          c("pcnt"),
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

    # procedures ----

    output$procedures_table <- gt::render_gt({
      shiny::validate(
        shiny::need(
          procedures_data,
          message = "Insufficient or suppressed data."
        )
      )

      pd <- procedures_data

      shiny::validate(
        shiny::need(
          !is.null(pd) && nrow(pd) > 0,
          "No procedures to display"
        )
      )

      strategy <- shiny::req(input$strategy)

      data <- pd |>
        dplyr::filter(
          .data$provider == params$dataset,
          .data$strategy == .env$strategy,
          .data$fyear == params$start_year
        ) |>
        dplyr::left_join(procedures_lkup, by = c("procedure_code" = "code")) |>
        tidyr::replace_na(list(
          description = "Unknown/Invalid Procedure Code"
        )) |>
        dplyr::select("procedure_description" = "description", "n", "pcnt")

      n_total <- sum(data$n)
      pcnt_total <- sum(data$pcnt)

      # if we need to include an other row
      if (pcnt_total < 1) {
        data <- dplyr::bind_rows(
          data,
          tibble::tibble(
            procedure_description = "Other",
            n = n_total * (1 - pcnt_total) / pcnt_total,
            pcnt = 1 - pcnt_total
          )
        )
      }

      gt::gt(data, "procedure_description") |>
        gt::cols_label(
          "procedure_description" = "Procedure",
          "n" = "Count of Activity (spells)",
          "pcnt" = "% of Total Activity"
        ) |>
        gt::tab_stubhead("Procedure") |>
        gt::fmt_number(
          c("n"),
          decimals = 0,
          use_seps = TRUE
        ) |>
        gt::fmt_percent(
          c("pcnt"),
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
              rows = .data$procedure_description == "Other"
            ),
            gt::cells_stub(
              rows = .data$procedure_description == "Other"
            )
          )
        )
    })

    # age group ----

    output$age_grp_plot <- shiny::renderPlot({
      strategy <- shiny::req(input$strategy)
      age_data <- age_sex_data |>
        dplyr::filter(
          .data$provider == params$dataset,
          .data$strategy == .env$strategy,
          .data$fyear == params$start_year
        )

      shiny::req(nrow(age_data) > 0)
      age_pyramid(age_data)
    })

    # NEE result ----

    output$nee_result <- shiny::renderPlot(
      {
        nee_params <- app_sys("app", "data", "nee_table.csv") |>
          readr::read_csv(col_types = "cddd") |>
          dplyr::filter(.data[["param_name"]] == input$strategy)

        shiny::validate(
          shiny::need(
            nrow(nee_params) > 0,
            "This TPMA was not part of the National Elicitation Exercise,
                      so a nationally-determined estimate is not available."
          )
        )

        nee_params |>
          ggplot2::ggplot() +
          ggplot2::geom_segment(
            ggplot2::aes(
              y = 1,
              yend = 1,
              x = .data[["percentile10"]],
              xend = .data[["percentile90"]]
            ),
            linewidth = 2
          ) +
          ggplot2::geom_point(
            ggplot2::aes(y = 1, x = mean),
            size = 5,
            colour = "#f9bf14"
          ) +
          ggplot2::xlim(0, 100) +
          ggplot2::xlab(
            "80% prediction interval (mean represented as a point)"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
          ) +
          ggplot2::ggtitle("Nationally-determined estimate (2039/40 horizon)")
      },
      width = "auto",
      height = 60
    )

    # rate values ----

    output$slider_absolute <- shiny::renderUI({
      strategy <- shiny::req(input$strategy)
      baseline_value <- provider_max_value()

      number_format <- config$interval_text_number_type %||% config$number_type

      # convert the slider values to absolute values
      interval_str <- number_format(
        slider_values[[mitigators_type]][[strategy]]$interval * baseline_value
      )

      rate_baseline <- number_format(baseline_value)
      y_axis_title <- config$y_axis_title

      text <- glue::glue(
        "This is equivalent to a rate interval of {interval_str[[1]]} to",
        "{interval_str[[2]]}",
        "({y_axis_title}) given the baseline of {rate_baseline}.",
        .sep = " "
      )

      style <- ifelse(input$include, "", "color: #ADAEAF;")

      shiny::tags$p(shiny::HTML(text), style = style)
    })

    output$slider_interval_text <- shiny::renderUI({
      text <- glue::glue(
        "Adjusting this slider will change the width of the",
        "corresponding yellow-highlighted region in the trend, funnel",
        "and boxplot charts above.",
        .sep = " "
      )

      style <- ifelse(input$include, "", "color: #ADAEAF;")

      shiny::tags$p(shiny::HTML(text), style = style)
    })
  })
}
