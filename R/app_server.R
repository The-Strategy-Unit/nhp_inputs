#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  diagnoses_lkup <- shiny::reactive({
    readRDS(app_sys("app", "data", "diagnoses.Rds"))
  })

  providers <- shiny::reactive({
    readRDS(app_sys("app", "data", "providers.Rds"))
  })

  params <- start <- NULL
  c(params, start) %<-% mod_home_server("home", providers())

  # load all other modules once the home module has finished loading
  init <- shiny::observe({
    shiny::req(start() > 0)

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

    mod_baseline_adjustment_server("baseline_adjustment", params)
    mod_covid_adjustment_server("covid_adjustment", params)

    mod_population_growth_server("population_growth", params)

    mod_inequalities_server("inequalities", params)

    mod_health_status_adjustment_server("health_status_adjustment", params)

    mod_waiting_list_imbalances_server("waiting_list_imbalances", params)
    mod_expat_repat_server("expat_repat", params, providers())

    mod_non_demographic_adjustment_server("non_demographic_adjustment", params)

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

    # enable the run_model page for certain users/running locally
    is_local <- Sys.getenv("SHINY_PORT") == ""
    is_power_user <- any(c("nhp_devs", "nhp_power_users") %in% session$groups)
    if (is_local || is_power_user) {
      shinyjs::show("run-model-container")
      mod_run_model_server("run_model", params)
    }

    # hacky way of achieving switch from the home tab to the app itself
    # maybe add some timeout to this?
    shinyjs::removeClass("tab-tab_home", "active")
    shinyjs::removeClass("shiny-tab-tab_home", "active")
    shinyjs::addClass("tab-tab_baseline_adjustment", "active")
    shinyjs::addClass("shiny-tab-tab_baseline_adjustment", "active")

    init$destroy()
  }) |>
    shiny::bindEvent(start())

  shiny::observe({
    shiny::req(start() > 0)
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
      mod_run_model_fix_params() |>
      jsonlite::write_json(file, pretty = TRUE, auto_unbox = TRUE)
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
