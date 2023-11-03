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

    # helpers ----
    extract_expat_repat_data <- function(k) {
      at <- shiny::req(input$activity_type)
      st <- shiny::req(input$ip_subgroup)
      t <- shiny::req(input$type)

      expat_repat_data()[[k]][[at]] |>
        dplyr::filter(
          if (at == "ip") .data$admigroup == st else TRUE,
          if (at == "aae") .data$is_ambulance == (t == "ambulance") else .data$specialty == t
        )
    }

    # reactives ----

    # load all of the expat repat data for this dataset
    expat_repat_data <- shiny::reactive({
      ds <- shiny::req(params$dataset)
      load_rds_from_adls(glue::glue("{ds}/expat_repat.rds"))
    }) |>
      shiny::bindCache(params$dataset)

    # extract the expat data for the current selection
    expat <- shiny::reactive({
      extract_expat_repat_data("expat") |>
        dplyr::select("fyear", "n")
    })

    # extract the repat local data for the current selection
    repat_local <- shiny::reactive({
      extract_expat_repat_data("repat_local") |>
        dplyr::select("fyear", "provider", "n", "pcnt")
    })

    # extract the repat nonlocal data for the current selection
    repat_nonlocal <- shiny::reactive({
      extract_expat_repat_data("repat_nonlocal") |>
        dplyr::filter(
          .data$fyear > 201415 # DQ issue: no rows prior to 15/16 have CCG
        ) |>
        dplyr::group_by(dplyr::across(-c("n", "icb22cdh"))) |>
        dplyr::mutate(pcnt = .data$n / sum(.data$n)) |>
        dplyr::ungroup() |>
        dplyr::select("fyear", "icb22cdh", "n", "pcnt")
    })

    # calculate the split between local and non-local activity by year
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
    # this way, we can keep track of where someone set a slider to, even if they then decide to not include it
    shadow_params <- shiny::reactiveValues()

    # observers ----

    # when the module is initialised, load the values from the loaded params file
    init <- shiny::observe(
      {
        p <- shiny::isolate({
          params
        })

        # update the selected time profile (all 3 will have the same value, so just use expat)
        update_time_profile(p$time_profile_mappings[["expat"]])

        default_values <- list(
          expat = c(0.95, 1.0),
          repat_local = c(1.0, 1.05),
          repat_nonlocal = c(1.0, 1.05)
        )

        # copy the values of the params to shadow params
        c(
          tidyr::expand_grid(
            type = c("expat", "repat_local", "repat_nonlocal"),
            activity_type = list(
              list(
                c("ip", "elective"),
                c("ip", "non-elective"),
                c("ip", "maternity"),
                "op"
              )
            ),
            specialty = rtt_specialties
          ) |>
            tidyr::unnest("activity_type") |>
            purrr::pmap(purrr::compose(unname, c)),
          list(c("expat", "aae", "ambulance")),
          list(c("expat", "aae", "walk-in"))
        ) |>
          purrr::walk(\(.x) {
            # if a value does exist in the params fallback to the default values
            # for that type
            v <- purrr::pluck(p, !!!.x) %||% default_values[[.x[[1]]]]
            purrr::pluck(shadow_params, !!!.x) <- v
          })

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
        # if the subgroup is maternity, just show the one specialty
        type_values <- if (at == "ip" && input$ip_subgroup == "maternity") {
          "Other (Medical)"
        } else {
          rtt_specialties
        }
      }
      shiny::updateSelectInput(session, "type", type_label, type_values)

      # reset the subgroup selection if we aren't on inpatients
      if (at != "ip") {
        shiny::updateSelectInput(session, "ip_subgroup", selected = "elective")
      }
    }) |>
      shiny::bindEvent(input$activity_type, input$ip_subgroup)


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

    # set up the observers for the sliders/checkboxes
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
        "Percentange of ICB's activity Delivered by this Provider",
        scale = 10
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
        input$include_repat_nonlocal,
        input$repat_nonlocal,
        params$start_year,
        "Percentange of Non-Local ICBs Activity",
        scale = 100
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
