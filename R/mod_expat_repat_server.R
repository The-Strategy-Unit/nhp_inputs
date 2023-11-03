#' expat_repat Server Functions
#'
#' @noRd
mod_expat_repat_server <- function(id, params, providers) { # nolint: object_usage_linter.
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  mod_reasons_server(shiny::NS(id, "reasons"), params, "expat_repat")

  shiny::moduleServer(id, function(input, output, session) {
    # static data ----
    rtt_specialties <- readRDS(app_sys("app", "data", "rtt_specialties.Rds"))
    icb_boundaries <- sf::read_sf(app_sys("app", "data", "icb_boundaries.geojson"))

    # reactives ----
    expat_repat_data <- shiny::reactive({
      ds <- shiny::req(params$dataset)
      load_rds_from_adls(glue::glue("{ds}/expat_repat.rds"))
    }) |>
      shiny::bindCache(params$dataset)

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
    shadow_params <- shiny::reactiveValues()

    # observers ----

    # update values when a file is uploaded
    init <- shiny::observe(
      {
        p <- shiny::isolate({
          params
        })

        shadow_params[["expat"]] <- params[["expat"]]
        shadow_params[["repat_local"]] <- params[["repat_local"]]
        shadow_params[["repat_nonlocal"]] <- params[["repat_nonlocal"]]

        # update the selected time profile (all 3 will have the same value, so just use expat)
        update_time_profile(p$time_profile_mappings[["expat"]])

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

        init$destroy()
      },
      priority = 10 # this observer needs to trigger before the dropdown change observer
    )

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
      shiny::bindEvent(input$activity_type, input$ip_subgroup, input$type)

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

    # renders ----

    output$repat_local_plot <- shiny::renderPlot({
      df <- repat_local() |>
        dplyr::filter(.data$provider == params$dataset)

      shiny::req(nrow(df) > 0)

      mod_expat_repat_trend_plot(
        df,
        input$include_repat_local,
        input$repat_local,
        params$start_year,
        "Percentange of ICB's activity Delivered by this Provider"
      )
    })

    output$repat_local_split_plot <- shiny::renderPlot({
      df <- repat_local() |>
        dplyr::filter(.data$fyear == params$start_year)

      shiny::req(nrow(df) > 0)

      mod_expat_repat_local_split_plot(
        df,
        providers,
        params$dataset,
        params$start_year
      )
    })

    output$repat_nonlocal_pcnt_plot <- shiny::renderPlot({
      df <- repat_local_nonlocal_split() |>
        dplyr::rename(pcnt = "nonlocal")

      shiny::req(nrow(df) > 0)

      mod_expat_repat_trend_plot(
        df,
        input$include_repat_local,
        input$repat_local,
        params$start_year,
        "Percentange of Non-Local ICBs Activity"
      )
    })

    output$repat_nonlocal_n <- shiny::renderPlot({
      df <- repat_nonlocal() |>
        dplyr::count(.data[["fyear"]], wt = .data[["n"]])

      shiny::req(nrow(df) > 0)

      mod_expat_repat_nonlocal_n(df)
    })

    output$repat_nonlocal_icb_map <- leaflet::renderLeaflet({
      df <- icb_boundaries |>
        dplyr::inner_join(
          repat_nonlocal() |>
            dplyr::filter(.data$fyear == params$start_year),
          by = "icb22cdh"
        )

      shiny::req(nrow(df) > 0)

      mod_expat_repat_nonlocal_icb_map(df)
    })

    # return ----
    NULL
  })
}
