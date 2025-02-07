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

  procedures_lkup <- shiny::reactive({
    readRDS(app_sys("app", "data", "procedures.Rds"))
  })

  mitigator_codes_lkup <- shiny::reactive({

    lkup <- readRDS(app_sys("app", "data", "mitigator-codes.Rds"))

    purrr::set_names(
      paste0(lkup[["strategy_name"]], " (", lkup[["mitigator_code"]], ")"),
      lkup[["strategy"]]
    )

  })

  providers <- shiny::reactive({
    readRDS(app_sys("app", "data", "providers.Rds"))
  })

  peers <- shiny::reactive({
    readRDS(app_sys("app", "data", "peers.Rds"))
  })

  params <- mod_home_server(
    "home",
    providers(),
    shiny::reactive(input$params_file)
  )

  # we could probably drop the need for start now, kept for historical reasons
  start <- shiny::reactive({
    shiny::req(length(params) > 0)
    shiny::req(params$dataset)
    shiny::req(params$scenario)
    1
  })

  # load all other modules once the home module has finished loading
  init <- shiny::observe({
    shiny::req(start() > 0)

    rates_data <- shiny::reactive({
      load_provider_data("rates")
    }) |>
      shiny::bindCache(params$dataset)

    age_sex_data <- shiny::reactive({
      age_sex <- load_provider_data("age_sex")
      age_fct <- age_sex |> _[["age_group"]] |> unique() |> sort()
      age_sex |> dplyr::mutate(age_group = factor(age_group, levels = age_fct))
    }) |>
      shiny::bindCache(params$dataset)

    diagnoses_data <- shiny::reactive({
      load_provider_data("diagnoses")
    }) |>
      shiny::bindCache(params$dataset)

    procedures_data <- shiny::reactive({
      load_provider_data("procedures")
    }) |>
      shiny::bindCache(params$dataset)

    available_strategies <- shiny::reactive({
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))

      rates_data() |>
        dplyr::filter(provider == dataset, fyear == year) |>
        _$strategy |>
        unique()
    }) |>
      shiny::bindCache(params$dataset, params$start_year)

    mod_baseline_adjustment_server("baseline_adjustment", params)
    mod_covid_adjustment_server("covid_adjustment", params)

    mod_population_growth_server("population_growth", params)

    # mod_inequalities_server("inequalities", params)

    mod_health_status_adjustment_server("health_status_adjustment", params)

    mod_waiting_list_imbalances_server("waiting_list_imbalances", params)
    mod_expat_repat_server("expat_repat", params, providers())

    mod_non_demographic_adjustment_server("non_demographic_adjustment", params)

    mod_mitigators_summary_server("mitigators_summary", age_sex_data(), params)

    purrr::walk(
      c(
        "mitigators_admission_avoidance",
        "mitigators_mean_los_reduction",
        "mitigators_aec_los_reduction",
        "mitigators_preop_los_reduction",
        "mitigators_day_procedures_daycase",
        "mitigators_day_procedures_outpatients",
        "mitigators_op_c2c_reduction",
        "mitigators_op_convert_tele",
        "mitigators_op_fup_reduction",
        "mitigators_op_gp_referred_first_attendance_reduction",
        "mitigators_aae_discharged_no_treatment",
        "mitigators_aae_frequent_attenders",
        "mitigators_aae_left_before_seen",
        "mitigators_aae_low_cost_discharged"
      ),
      mod_mitigators_server,
      params,
      rates_data(),
      age_sex_data(),
      diagnoses_data(),
      procedures_data(),
      available_strategies,
      diagnoses_lkup(),
      procedures_lkup(),
      mitigator_codes_lkup(),
      peers()
    )

    # enable the run_model page for certain users/running locally

    can_run_model <- any(c("nhp_devs", "nhp_power_users", "nhp_run_model") %in% session$groups)
    if (is_local() || can_run_model) {
      shinyjs::show("run-model-container")
      mod_run_model_server("run_model", params)
    }

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
