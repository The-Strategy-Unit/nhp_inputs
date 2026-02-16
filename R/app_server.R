#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  params <- mod_home_server(
    "home",
    shiny::reactive(input$params_file)
  )

  # we could probably drop the need for start now, kept for historical reasons
  start <- shiny::reactive({
    shiny::req(length(params) > 0)
    shiny::req(params$dataset)
    shiny::req(params$scenario)
    1
  })

  # load all data
  rates_data <- shiny::reactive({
    get_rates_data()
  })

  age_sex_data <- shiny::reactive({
    get_age_sex_data()
  })

  diagnoses_data <- shiny::reactive({
    get_diagnoses_data()
  })

  procedures_data <- shiny::reactive({
    get_procedures_data()
  })

  baseline_data <- shiny::reactive({
    get_baseline_data()
  })

  wli_data <- shiny::reactive({
    get_wli_data()
  })

  inequalities_data <- shiny::reactive({
    get_inequalities_data()
  })

  expat_data <- shiny::reactive({
    get_expat_data()
  })

  repat_local_data <- shiny::reactive({
    get_repat_local_data()
  })

  repat_nonlocal_data <- shiny::reactive({
    get_repat_nonlocal_data()
  })

  # load all other modules once the home module has finished loading
  init <- shiny::observe({
    shiny::req(start() > 0)

    available_strategies <- shiny::reactive({
      # nolint start: object_usage_linter
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))
      # nolint end

      rates_data() |>
        dplyr::filter(
          .data[["provider"]] == .env[["dataset"]],
          .data[["fyear"]] == .env[["year"]]
        ) |>
        dplyr::filter(
          .by = "strategy",
          sum(.data[["denominator"]]) > 5
        ) |>
        _$strategy |>
        unique()
    })

    mod_baseline_adjustment_server(
      "baseline_adjustment",
      baseline_data(),
      params
    )

    mod_population_growth_server("population_growth", params)

    mod_health_status_adjustment_server("health_status_adjustment", params)

    shiny::observe({
      can_set_inequalities <- any(
        c("nhp_devs", "nhp_power_users", "nhp_test_inequalities") %in%
          session$groups
      )

      shinyjs::toggle(
        "inequalities_tab",
        condition = (is_local() || can_set_inequalities)
      )
    })

    mod_inequalities_server("inequalities", inequalities_data(), params)

    mod_waiting_list_imbalances_server(
      "waiting_list_imbalances",
      wli_data(),
      params
    )

    mod_expat_repat_server(
      "expat_repat",
      expat_data(),
      repat_local_data(),
      repat_nonlocal_data(),
      params
    )

    mod_non_demographic_adjustment_server("non_demographic_adjustment", params)

    mod_mitigators_summary_server("mitigators_summary", age_sex_data(), params)

    purrr::walk(
      c(
        "mitigators_admission_avoidance",
        "mitigators_mean_los_reduction",
        "mitigators_sdec_conversion",
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
      available_strategies
    )

    # enable the run_model page for certain users/running locally

    can_run_model <- any(
      c("nhp_devs", "nhp_power_users", "nhp_run_model") %in% session$groups
    )
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
      session$user %||% "[development]",
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
