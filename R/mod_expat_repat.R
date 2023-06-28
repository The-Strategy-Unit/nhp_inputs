#' expat_repat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expat_repat_ui <- function(id) {
  ns <- shiny::NS(id)

  generate_param_controls <- function(type, min, max, values) {
    shiny::fluidRow(
      col_3(
        shiny::checkboxInput(ns(glue::glue("include_{type}")), "Include?")
      ),
      col_9(
        shinyjs::disabled(
          shiny::sliderInput(
            ns(type),
            "Confidence Interval",
            min, max, values, 0.1,
            post = "%"
          )
        )
      )
    )
  }

  shiny::tagList(
    shiny::tags$h1("Expatriation/Repatriation"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          title = "Selection",
          width = 12,
          shiny::selectInput(
            ns("activity_type"),
            "Activity Type",
            c(
              "Inpatients" = "ip",
              "Outpatients" = "op",
              "A&E" = "aae"
            )
          ),
          shinyjs::hidden(
            shiny::selectInput(
              ns("ip_subgroup"),
              "Subgroup",
              c(
                "Elective" = "elective",
                "Non-Elective" = "non-elective",
                "Maternity" = "maternity"
              )
            )
          ),
          shiny::selectInput(
            ns("type"),
            NULL,
            NULL
          )
        ),
        mod_time_profile_ui(ns("time_profile")),
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "expat_repat.md")
        ),
      ),
      col_8(
        bs4Dash::box(
          title = "Expatriation Model Parameter",
          width = 12,
          generate_param_controls("expat", 0, 100, c(95, 100))
        ),
        bs4Dash::box(
          title = "Repatriation (Local) Model Parameter",
          width = 12,
          generate_param_controls("repat_local", 100, 200, c(100, 105)),
          shiny::fluidRow(
            col_6(
              shiny::plotOutput(
                ns("repat_local_plot"),
              )
            ),
            col_6(
              shiny::plotOutput(
                ns("repat_local_split_plot")
              )
            )
          )
        ),
        bs4Dash::box(
          title = "Repatriation (Non-Local) Model Parameter",
          width = 12,
          generate_param_controls("repat_nonlocal", 100, 200, c(100, 105)),
          shiny::fluidRow(
            col_4(
              shiny::plotOutput(
                ns("repat_nonlocal_pcnt_plot")
              )
            ),
            col_4(
              shiny::plotOutput(
                ns("repat_nonlocal_n")
              )
            ),
            col_4(
              leaflet::leafletOutput(
                ns("repat_nonlocal_icb_map")
              )
            )
          )
        )
      )
    )
  )
}

#' expat_repat Server Functions
#'
#' @noRd
mod_expat_repat_server <- function(id, params, providers) { # nolint: object_usage_linter.
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  shiny::moduleServer(id, function(input, output, session) {
    rtt_specialties <- readRDS(app_sys("app", "data", "rtt_specialties.Rds"))
    icb_boundaries <- sf::read_sf(app_sys("app", "data", "icb_boundaries.geojson"))

    expat_repat_data <- shiny::reactive({
      ds <- shiny::req(params$dataset)
      load_rds_from_adls(glue::glue("{ds}/expat_repat.rds"))
    }) |>
      shiny::bindCache(params$dataset)

    # helper method to construct the initial values for our params
    init_params <- function(values) {
      if (missing(values)) {
        rtt_specs <- list()
        aae_groups <- list()
      } else {
        rtt_specs <- purrr::map(purrr::set_names(rtt_specialties), ~values)
        aae_groups <- list(
          "ambulance" = values,
          "walk-in" = values
        )
      }

      list(
        "ip" = purrr::map(
          purrr::set_names(c("elective", "non-elective", "maternity")),
          ~rtt_specs,
        ),
        "op" = rtt_specs,
        "aae" = aae_groups
      )
    }

    # update the time profile
    shiny::observe({
      params$time_profile_mappings[
        c(
          "expat",
          "repat_local",
          "repat_nonlocal"
        )
      ] <- selected_time_profile()
    }) |>
      shiny::bindEvent(selected_time_profile())

    # two reactiveValues to keep track of the slider values
    # shadow_params always stores a value for each item selectable by the dropdowns
    # params contains the returned values, and will contain the value from shadow_params if "include" is checked
    shadow_params <- purrr::lift_dl(shiny::reactiveValues)(
      list(
        "expat" = init_params(c(0.95, 1.0)),
        "repat_local" = init_params(c(1.0, 1.05)),
        "repat_nonlocal" = init_params(c(1.0, 1.05))
      )
    )
    params$expat <- init_params()
    params$repat_local <- init_params()
    params$repat_nonlocal <- init_params()

    # update values when a file is uploaded
    shiny::observe(
      {
        shiny::req(session$userData$data_loaded())
        p <- shiny::req(session$userData$params)

        # update the selected time profile (all 3 will have the same value, so just use expat)
        update_time_profile(session$userData$params$time_profile_mappings[["expat"]])

        tidyr::expand_grid(
          type = names(shadow_params),
          specialty = rtt_specialties
        ) |>
          purrr::pwalk(\(type, specialty) {
            purrr::walk(c("elective", "non-elective", "maternity"), \(.y) {
              v <- p[[type]]$ip[[.y]][[specialty]]
              shadow_params[[type]]$ip[[.y]][[specialty]] <- v %||% c(0.95, 1.0)
              params[[type]]$ip[[.y]][[specialty]] <- v
            })
            v <- p[[type]]$op[[specialty]]
            shadow_params[[type]]$op[[specialty]] <- v %||% c(0.95, 1.0)
            params[[type]]$op[[specialty]] <- v
          })

        tidyr::expand_grid(
          type = names(shadow_params),
          arrival_mode = names(shadow_params$expat$aae)
        ) |>
          purrr::pwalk(\(type, arrival_mode) {
            v <- p[[type]]$aae[[arrival_mode]]
            shadow_params[[type]]$aae[[arrival_mode]] <- v %||% c(0.95, 1.0)
            params[[type]]$aae[[arrival_mode]] <- v
          })

        default_spec <- rtt_specialties[[1]]
        shiny::updateSelectInput(session, "activity_type", selected = "ip")
        shiny::updateSelectInput(session, "ip_subgroup", selected = "elective")
        shiny::updateSelectInput(session, "type", selected = default_spec)
      },
      priority = 10 # this observer needs to trigger before the dropdown change observer
    ) |>
      shiny::bindEvent(session$userData$data_loaded())

    # update the options in the type drop down based on the activity type dropdown
    # also, toggle whether the ip_subgroup is visible or not
    shiny::observe({
      at <- shiny::req(input$activity_type)

      shinyjs::toggle("ip_subgroup", condition = at == "ip")

      if (at == "aae") {
        type_label <- "Attendance Type"
        type_values <- c(
          "Ambulance" = "ambulance",
          "Walk-In" = "walk-in"
        )
      } else {
        type_label <- "Specialty"
        type_values <- rtt_specialties
      }
      shiny::updateSelectInput(session, "type", type_label, type_values)
    }) |>
      shiny::bindEvent(input$activity_type)

    # watch for changes to the dropdowns
    # update the sliders to the values for the combination of the drop downs in shadow_params
    # set the include checkboxes value if a value exists in params or not
    shiny::observe({
      purrr::walk(
        c("expat", "repat_local", "repat_nonlocal"),
        \(type) {
          at <- shiny::req(input$activity_type)
          st <- shiny::req(input$ip_subgroup)
          t <- shiny::req(input$type)

          sp <- shadow_params[[type]][[at]]
          p <- params[[type]][[at]]
          if (at == "ip") {
            sp <- sp[[st]]
            p <- p[[st]]
          }
          sp <- sp[[t]]
          p <- p[[t]]

          shiny::updateCheckboxInput(session, glue::glue("include_{type}"), value = !is.null(p))
          shiny::updateSliderInput(session, type, value = sp * 100)
        }
      )
    }) |>
      shiny::bindEvent(input$activity_type, input$ip_subgroup, input$type, session$userData$data_loaded())

    purrr::walk(
      c("expat", "repat_local", "repat_nonlocal"),
      \(type) {
        include_type <- glue::glue("include_{type}")

        # watch the slider values and the include check boxes
        # if the slider value changes then we update the value of the shadow_params to the new slider values
        # set the params to be the slider values if include is checked
        # if it is checked, set the value to null (i.e. delete it from the list)
        shiny::observe({
          at <- shiny::req(input$activity_type)
          st <- shiny::req(input$ip_subgroup)
          t <- shiny::req(input$type)

          include <- input[[include_type]]
          v <- shiny::req(input[[type]]) / 100

          if (at == "ip") {
            shadow_params[[type]][[at]][[st]][[t]] <- v

            params[[type]][[at]][[st]][[t]] <- if (include) v
          } else {
            shadow_params[[type]][[at]][[t]] <- v
            params[[type]][[at]][[t]] <- if (include) v
          }
        }) |>
          shiny::bindEvent(input[[type]], input[[include_type]])

        shiny::observe({
          shinyjs::toggleState(type, condition = input[[include_type]])
        }) |>
          shiny::bindEvent(input[[include_type]])
      }
    )

    # data for charts
    expat <- shiny::reactive({
      at <- shiny::req(input$activity_type)
      st <- shiny::req(input$ip_subgroup)
      t <- shiny::req(input$type)

      expat_repat_data()$expat[[at]] |>
        dplyr::filter(
          if (at == "ip") .data$admigroup == st else TRUE,
          if (at == "aae") .data$is_ambulance == (t == "ambulance") else .data$specialty == t
        ) |>
        dplyr::select("fyear", "n")
    })

    repat_local <- shiny::reactive({
      at <- shiny::req(input$activity_type)
      st <- shiny::req(input$ip_subgroup)
      t <- shiny::req(input$type)

      expat_repat_data()$repat_local[[at]] |>
        dplyr::filter(
          if (at == "ip") .data$admigroup == st else TRUE,
          if (at == "aae") .data$is_ambulance == (t == "ambulance") else .data$specialty == t
        ) |>
        dplyr::select("fyear", "provider", "n", "pcnt")
    })

    repat_nonlocal <- shiny::reactive({
      at <- shiny::req(input$activity_type)
      st <- shiny::req(input$ip_subgroup)
      t <- shiny::req(input$type)

      expat_repat_data()$repat_nonlocal[[at]] |>
        dplyr::filter(
          .data$fyear > 201415, # DQ issue: no rows prior to 15/16 have CCG
          if (at == "ip") .data$admigroup == st else TRUE,
          if (at == "aae") .data$is_ambulance == (t == "ambulance") else .data$specialty == t
        ) |>
        dplyr::group_by(dplyr::across(-c("n", "icb22cdh"))) |>
        dplyr::mutate(pcnt = .data$n / sum(.data$n)) |>
        dplyr::ungroup() |>
        dplyr::select("fyear", "icb22cdh", "n", "pcnt")
    })

    repat_local_nonlocal_split <- shiny::reactive({
      repat_local() |>
        dplyr::filter(.data$provider == params$dataset) |>
        dplyr::count(.data$fyear, wt = .data$n, name = "d") |>
        dplyr::inner_join(expat(), by = "fyear") |>
        dplyr::transmute(
          .data$fyear,
          local = .data$d / .data$n,
          nonlocal = 1 - .data$local
        )
    })

    output$repat_local_plot <- shiny::renderPlot({
      df <- repat_local() |>
        dplyr::filter(.data$provider == params$dataset)

      shiny::req(nrow(df) > 0)

      v <- dplyr::filter(df, .data$fyear == params$start_year)$pcnt

      include <- input$include_repat_local
      values <- input$repat_local * v / 100

      interval <- if (include) {
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
      }

      df |>
        ggplot2::ggplot(ggplot2::aes(as.factor(.data$fyear), .data$pcnt, group = 1)) +
        interval +
        ggplot2::geom_line() +
        ggplot2::geom_point(
          data = \(.x) dplyr::filter(.x, .data$fyear == params$start_year),
          colour = "red"
        ) +
        ggplot2::scale_x_discrete(
          labels = \(.x) stringr::str_replace(.x, "^(\\d{4})(\\d{2})$", "\\1/\\2")
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent,
          limits = c(floor(min(df$pcnt * 10)), ceiling(v * 20)) / 10
        ) +
        ggplot2::labs(
          x = "Financial Year",
          y = "Percentange of ICB's activity Delivered by this Provider"
        ) +
        ggplot2::theme(
          legend.position = "none",
          panel.background = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
        )
    })

    output$repat_local_split_plot <- shiny::renderPlot({
      providers_df <- tibble::enframe(
        providers,
        value = "provider",
        name = "provider_name"
      )

      this_provider_name <- providers_df |>
        dplyr::filter(.data$provider == params$dataset) |>
        dplyr::pull(.data$provider_name)

      df <- repat_local() |>
        dplyr::filter(.data$fyear == params$start_year)

      shiny::req(nrow(df) > 0)

      # in the call to ggplot2::after_scale we will always get a check warning for no visible binding for colour:
      # create a value temporarily to hide this
      colour <- NULL

      df |>
        dplyr::left_join(providers_df, by = c("provider")) |>
        dplyr::arrange(.data$provider_name) |>
        dplyr::mutate(
          dplyr::across(
            "provider_name",
            \(.x) {
              .x |>
                forcats::fct_na_value_to_level("Other") |>
                forcats::fct_relevel(this_provider_name) |>
                forcats::fct_relevel("Other", after = Inf)
            }
          ),
          label = glue::glue(
            .sep = "\n",
            "{.data$provider_name}",
            "{scales::comma(.data$n)} ({scales::percent(.data$pcnt)})"
          ),
        ) |>
        dplyr::arrange(dplyr::desc(.data$provider_name)) |>
        dplyr::mutate(
          label_pos = cumsum(.data$n) - .data$n / 2
        ) |>
        ggplot2::ggplot(ggplot2::aes(1, .data$n)) +
        ggplot2::geom_col(
          ggplot2::aes(
            colour = .data$provider_name,
            fill = ggplot2::after_scale(ggplot2::alpha(colour, 0.4)) # nolint
          ),
          position = "stack"
        ) +
        ggplot2::geom_label(
          ggplot2::aes(y = .data$label_pos, label = .data$label),
          fill = "#ffffff"
        ) +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none")
    })

    output$repat_nonlocal_pcnt_plot <- shiny::renderPlot({
      df <- repat_local_nonlocal_split()

      shiny::req(nrow(df) > 0)

      df |>
        ggplot2::ggplot(ggplot2::aes(as.factor(.data$fyear), .data$nonlocal, group = 1)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
          data = \(.x) dplyr::filter(.x, .data$fyear == params$start_year),
          colour = "red"
        ) +
        ggplot2::scale_x_discrete(
          labels = \(.x) stringr::str_replace(.x, "^(\\d{4})(\\d{2})$", "\\1/\\2")
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent
        ) +
        ggplot2::labs(
          x = "Financial Year",
          y = "Percentange of Non-Local ICBs Activity"
        ) +
        ggplot2::theme(
          legend.position = "none",
          panel.background = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
        )
    })

    output$repat_nonlocal_n <- shiny::renderPlot({
      df <- repat_nonlocal()

      shiny::req(nrow(df) > 0)

      df |>
        ggplot2::ggplot(
          ggplot2::aes(
            as.factor(.data$fyear),
            .data$n,
            color = .data$icb22cdh,
            fill = ggplot2::after_scale(ggplot2::alpha(colour, 0.4)) # nolint
          )
        ) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::scale_x_discrete(
          labels = \(.x) stringr::str_replace(.x, "^(\\d{4})(\\d{2})$", "\\1/\\2")
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::comma
        ) +
        ggplot2::labs(
          x = "Financial Year",
          y = "Number of spells delivered to non-local ICB residents"
        ) +
        ggplot2::theme(
          legend.position = "none",
          panel.background = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
        )
    })

    output$repat_nonlocal_icb_map <- leaflet::renderLeaflet({
      df <- icb_boundaries |>
        dplyr::inner_join(
          repat_nonlocal() |>
            dplyr::filter(.data$fyear == params$start_year),
          by = "icb22cdh"
        )

      shiny::req(nrow(df) > 0)

      pal <- leaflet::colorNumeric( # nolint
        viridis::viridis_pal()(3),
        df$pcnt
      )

      leaflet::leaflet(df) |>
        leaflet::addProviderTiles("Stamen.TonerLite") |>
        leaflet::addPolygons(
          color = "#000000",
          weight = 1,
          opacity = 1,
          fillColor = ~ pal(pcnt),
          popup = ~ glue::glue("{icb22nm}: {n} ({scales::percent(pcnt)})")
        )
    })
  })
}
