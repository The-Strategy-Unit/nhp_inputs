#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # load reference data - make sure to cache these, but they need a dummy cache
  # key, so we use a scalar value.
  diagnoses_lkup <- shiny::reactive({
    readRDS(app_sys("app", "data", "diagnoses.Rds"))
  }) |>
    shiny::bindCache(1)

  providers <- shiny::reactive({
    readRDS(app_sys("app", "data", "providers.Rds"))
  }) |>
    shiny::bindCache(1)

  params <- shiny::reactiveValues()
  params[["demographic_factors"]] <- list(
    file = "demographic_factors.csv"
  )
  params[["time_profile_mappings"]] <- list(
    "covid_adjustment" = "none",
    "baseline_adjustment" = "none",
    "bed_occupancy" = "none",
    "theatres" = "none",
    "activity_avoidance" = list(
      "ip" = list(),
      "op" = list(),
      "aae" = list()
    ),
    "efficiencies" = list(
      "ip" = list(),
      "op" = list()
    )
  )

  session$userData$data_loaded <- shiny::reactiveVal()

  start <- mod_home_server("home", providers(), params)

  shiny::observe(
    {
      shiny::req(start() > 0)

      # hacky way of achieving switch from the home tab to the app itself
      shinyjs::removeClass("tab-tab_home", "active")
      shinyjs::removeClass("shiny-tab-tab_home", "active")
      shinyjs::addClass("tab-tab_baseline_adjustment", "active")
      shinyjs::addClass("shiny-tab-tab_baseline_adjustment", "active")

      if (!is.null(session$userData$params)) {
        session$userData$data_loaded(Sys.time())

        # some of the modules do not properly update - forcing the evaluation of the params fixes this
        shiny::reactiveValuesToList(params)
      }
    },
    priority = -1
  ) |>
    shiny::bindEvent(start())

  # load all other modules once the home module has finished loading
  init_timeout <- TRUE
  init <- shiny::observe({
    shiny::req(params$dataset)
    if (init_timeout) {
      shiny::invalidateLater(50)
      shiny::req((init_timeout <<- FALSE))
    }

    shiny::isolate({
      provider_data <- shiny::reactive({
        dataset <- shiny::req(params$dataset)
        file <- glue::glue("{dataset}/data.rds")

        load_rds_from_adls(file)
      }) |>
        shiny::bindCache(params$dataset)

      available_strategies <- shiny::reactive({
        dataset <- shiny::req(params$dataset)
        year <- as.character(shiny::req(params$start_year))

        load_rds_from_adls(glue::glue("{dataset}/available_strategies.rds"))[[year]]
      }) |>
        shiny::bindCache(params$dataset, params$start_year)

      mod_expat_repat_server("expat_repat", params, providers())

      mod_population_growth_server("population_growth", params)
      mod_health_status_adjustment_server("health_status_adjustment", params)

      mod_baseline_adjustment_server("baseline_adjustment", params)
      mod_non_demographic_adjustment_server("non_demographic_adjustment", params)
      mod_waiting_list_imbalances_server("waiting_list_imbalances", params)

      # currently, there is no corresponding UI for this
      mod_covid_adjustment_server("covid_adjustment", params)

      mod_theatres_server("theatres", params)
      mod_bed_occupancy_server("bed_occupancy", params)

      purrr::walk(
        c(
          "mitigators_admission_avoidance",
          "mitigators_mean_los_reduction",
          "mitigators_aec_los_reduction",
          "mitigators_preop_los_reduction",
          "mitigators_bads",
          "mitigators_op_c2c_reduction",
          "mitigators_op_convert_tele",
          "mitigators_op_fup_reduction",
          "mitigators_aae_frequent_attenders",
          "mitigators_aae_left_before_seen",
          "mitigators_aae_low_cost_discharged"
        ),
        mod_mitigators_server,
        params,
        provider_data,
        available_strategies,
        diagnoses_lkup()
      )

      mod_run_model_server("run_model", params)
    })

    shiny::observe({
      shiny::req(start() > 0)
      # TODO: this won't persist across redeployments
      shiny::req(params$dataset)
      shiny::req(params$scenario)

      file <- params_filename(
        # if running locally, then user will be NULL
        session$user %||% ".",
        params$dataset,
        params$scenario
      )

      params |>
        shiny::reactiveValuesToList() |>
        mod_run_model_fix_params(session$user) |>
        jsonlite::write_json(file, pretty = TRUE, auto_unbox = TRUE)
    })

    init$destroy()
  })


  if (as.logical(Sys.getenv("ENABLE_AUTO_RECONNECT", FALSE))) {
    cat("auto reconnect enabled\n")
    session$allowReconnect("force")
  }

  shiny::observe({
    shiny::req("nhp_devs" %in% session$groups)

    u <- shiny::parseQueryString(session$clientData$url_search)

    shiny::req(!is.null(u$reset_cache))
    cat("reset cache\n")

    dc <- shiny::shinyOptions()$cache

    dc$reset()
  })

  # return
  NULL
}
