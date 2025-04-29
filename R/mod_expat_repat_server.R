#' expat_repat Server Functions
#'
#' @noRd
mod_expat_repat_server <- function(id, params, providers) {
  selected_time_profile <- update_time_profile <- NULL
  # nolint start: object_usage_linter.
  c(selected_time_profile, update_time_profile) %<-%
    mod_time_profile_server(
      shiny::NS(id, "time_profile"),
      params
    )
  # nolint end

  mod_reasons_server(shiny::NS(id, "reasons"), params, "expat_repat")

  shiny::moduleServer(id, function(input, output, session) {
    # static data ----
    rtt_specialties <- readRDS(app_sys("app", "data", "rtt_specialties.Rds"))
    icb_boundaries <- sf::read_sf(app_sys(
      "app",
      "data",
      "icb_boundaries.geojson"
    ))

    # helpers ----

    extract_expat_repat_data <- function(dat) {
      # TODO: techdebt
      # we should rename the dropdowns
      # when op, set the group dropdown to ""
      # when aae, set the tretspef dropdown to "Other"
      at <- shiny::req(input$activity_type)
      st <- shiny::req(input$ip_subgroup) # < this should become tretspef
      t <- shiny::req(input$type) # < this should become group

      dat <- dplyr::filter(dat, .data[["activity_type"]] == .env[["at"]])

      if (at == "op") {
        return(dplyr::filter(dat, .data[["tretspef"]] == .env[["t"]]))
      }
      if (at == "aae") {
        return(dplyr::filter(dat, .data[["group"]] == .env[["t"]]))
      }
      dat |>
        dplyr::filter(
          .data$group == st,
          .data$tretspef == t
        )
    }

    # reactives ----

    # extract the expat data for the current selection
    expat_raw <- shiny::reactive({
      ds <- shiny::req(params$dataset)
      load_provider_data("expat") |>
        dplyr::filter(.data$provider == ds)
    })
    expat <- shiny::reactive({
      expat_raw() |>
        extract_expat_repat_data() |>
        dplyr::select("fyear", "count")
    })

    # extract the repat local data for the current selection
    repat_local_raw <- shiny::reactive({
      load_provider_data("repat_local")
    })
    repat_local <- shiny::reactive({
      repat_local_raw() |>
        extract_expat_repat_data() |>
        dplyr::select("fyear", "icb", "provider", "count", "pcnt")
    })

    # extract the repat nonlocal data for the current selection
    repat_nonlocal_raw <- shiny::reactive({
      load_provider_data("repat_nonlocal")
    })
    repat_nonlocal <- shiny::reactive({
      repat_nonlocal_raw() |>
        extract_expat_repat_data() |>
        dplyr::select(
          "fyear",
          "provider",
          "icb",
          "is_main_icb",
          "count",
          "pcnt"
        )
    })

    # calculate the split between local and non-local activity by year
    repat_local_nonlocal_split <- shiny::reactive({
      repat_local() |>
        dplyr::filter(.data$provider == params$dataset) |>
        dplyr::count(.data$fyear, wt = .data$count, name = "d") |>
        dplyr::inner_join(expat(), by = "fyear") |>
        dplyr::transmute(
          .data$fyear,
          local = .data$d / .data$count,
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
          tidyr::expand_grid(
            a = c("expat", "repat_local", "repat_nonlocal"),
            b = "aae",
            c = c("ambulance", "walk-in")
          ) |>
            purrr::pmap(purrr::compose(unname, c))
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
    shiny::observe(
      {
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
          shiny::updateSelectInput(
            session,
            "ip_subgroup",
            selected = "elective"
          )
        }
      },
      priority = 100
    ) |>
      shiny::bindEvent(input$activity_type, input$ip_subgroup)

    # watch for changes to the dropdowns
    # update the sliders to the values for the combination of the drop downs in shadow_params
    # set the include checkboxes value if a value exists in params or not
    shiny::observe(
      {
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

            shiny::req(sp)

            shiny::updateCheckboxInput(
              session,
              glue::glue("include_{type}"),
              value = !is.null(p)
            )
            shiny::updateSliderInput(session, type, value = sp * 100)
          }
        )
      },
      priority = 10
    ) |>
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
      focus_icb <- repat_local() |>
        dplyr::filter(
          .data$fyear == params$start_year,
          .data$provider == params$dataset
        ) |>
        dplyr::pull(.data$icb)

      df <- repat_local() |>
        dplyr::filter(
          .data$fyear == params$start_year,
          .data$icb == focus_icb
        )

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
        dplyr::filter(
          .data[["provider"]] == params$dataset,
          !.data[["is_main_icb"]]
        ) |>
        dplyr::count(.data[["fyear"]], wt = .data[["count"]])

      shiny::req(nrow(df) > 0)

      mod_expat_repat_nonlocal_n(df)
    })

    output$repat_nonlocal_icb_map <- leaflet::renderLeaflet({
      dat <- repat_nonlocal() |>
        dplyr::filter(
          .data$count > 5,
          .data$fyear == params$start_year,
          .data[["provider"]] == params$dataset,
          !.data[["is_main_icb"]]
        )

      df <- icb_boundaries |>
        dplyr::inner_join(
          dat,
          by = dplyr::join_by("icb22cdh" == "icb")
        )

      shiny::req(nrow(df) > 0)

      mod_expat_repat_nonlocal_icb_map(df)
    })

    # return ----
    NULL
  })
}
