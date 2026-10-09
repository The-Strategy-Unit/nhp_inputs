#' expat_repat Server Functions
#'
#' @noRd
mod_expat_repat_server <- function(id, params) {
  providers <- get_lookups()[["providers"]]

  specialties <- get_lookups()[["specialties"]] |>
    dplyr::select("specialty", "tretspef") |>
    tibble::deframe()

  mod_reasons_server(shiny::NS(id, "reasons"), params, "expat_repat")

  shiny::moduleServer(id, function(input, output, session) {
    expat_data <- shiny::reactive({
      get_expat_data(params$dataset)
    })

    repat_local_data <- shiny::reactive({
      get_repat_local_data()
    })

    repat_nonlocal_data <- shiny::reactive({
      get_repat_nonlocal_data()
    })

    valid_specialties <- shiny::reactive({
      at <- shiny::req(input$activity_type)

      if (at == "aae") {
        return(c("Other"))
      }

      st <- shiny::req(input$ip_subgroup)

      df <- expat_data() |>
        dplyr::filter(
          .data[["activity_type"]] == at,
          at == "op" | .data[["group"]] == st
        )

      print(df)

      specialties[specialties %in% unique(df[["tretspef"]])]
    })

    # helpers ----

    extract_expat_repat_data <- function(dat) {
      at <- shiny::req(input$activity_type)
      g <- shiny::req(input$group)
      t <- shiny::req(input$tretspef)

      dat |>
        dplyr::filter(
          .data$group == g,
          .data$tretspef == t
        )
    }

    # reactives ----

    # extract the expat data for the current selection
    expat <- shiny::reactive({
      expat_data() |>
        extract_expat_repat_data() |>
        dplyr::select("fyear", "count")
    })

    # extract the repat local data for the current selection
    repat_local <- shiny::reactive({
      repat_local_data() |>
        extract_expat_repat_data() |>
        dplyr::select("fyear", "icb", "provider", "count", "pcnt")
    })

    # extract the repat nonlocal data for the current selection
    repat_nonlocal <- shiny::reactive({
      repat_nonlocal_data() |>
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

        default_values <- list(
          expat = c(0.95, 1.0),
          repat_local = c(1.0, 1.05),
          repat_nonlocal = c(1.0, 1.05)
        )

        # copy the values of the params to shadow params
        dplyr::cross_join(
          tibble::tibble(type = c("expat", "repat_local", "repat_nonlocal")),
          expat_data() |>
            dplyr::filter(.data$fyear == params$start_year) |>
            dplyr::select("activity_type", "group", "tretspef")
        ) |>
          purrr::pmap(\(...) {
            dots <- list(...)
            # if a value does exist in the params fallback to the default values
            # for that type
            v <- purrr::pluck(p, !!!dots) %||% default_values[[dots[[1]]]]
            purrr::pluck(shadow_params, !!!dots) <- v
          })

        init$destroy()
      },
      priority = 10 # this observer needs to trigger before the dropdown change observer
    )

    shiny::observe(
      {
        at <- shiny::req(input$activity_type)

        shinyjs::toggle("group", condition = at != "op")
        shinyjs::toggle("tretspef", condition = at != "aae")

        shiny::updateSelectInput(
          session,
          "group",
          choices = switch(
            at,
            "ip" = c("elective", "non-elective", "maternity"),
            "op" = c(""),
            "aae" = c("ambulance", "walk-in")
          )
        )
      },
      priority = 100
    ) |>
      shiny::bindEvent(input$activity_type)

    shiny::observe(
      {
        at <- shiny::req(input$activity_type)
        g <- shiny::req(input$group)

        specialties_to_select <- expat_data() |>
          dplyr::filter(
            .data$fyear == params$start_year,
            .data$activity_type == at,
            .data$group == g
          ) |>
          _$tretspef |>
          unique()

        shiny::updateSelectInput(
          session,
          "tretsepf",
          choices = specialties[specialties %in% specialties_to_select]
        )
      },
      priority = 100
    ) |>
      shiny::bindEvent(input$activity_type, input$group)

    # Watch for changes to the dropdowns.
    # Update the sliders to the values for the combination of the drop downs
    # in shadow_params.
    # Set the include checkboxes value if a value exists in params or not.
    shiny::observe(
      {
        purrr::walk(
          c("expat", "repat_local", "repat_nonlocal"),
          \(type) {
            at <- shiny::req(input$activity_type)
            st <- shiny::req(input$group)
            t <- shiny::req(input$tretspef)

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
      shiny::bindEvent(input$activity_type, input$group, input$tretspef)

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
          st <- shiny::req(input$group)
          t <- shiny::req(input$tretspef)

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

    shiny::observe({
      icb_pcnts <- repat_nonlocal() |>
        dplyr::filter(
          .data[["count"]] > 5,
          .data[["fyear"]] == params$start_year,
          .data[["provider"]] == params$dataset,
          !.data[["is_main_icb"]]
        ) |>
        dplyr::select("icb", "pcnt") |>
        tibble::deframe() |>
        as.list()

      shiny::req(length(icb_pcnts) > 0)

      session$sendCustomMessage("selectedIcbs", icb_pcnts)
    })

    # return ----
    NULL
  })
}
