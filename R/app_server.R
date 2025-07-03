#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # in fct_create_data_cache, we utilise this env var to invalidate the cache
  # we can use it's value to allow us to cache all of the reactive data without
  # having to bind to some other input which might change
  cache_version <- shiny::reactive({
    Sys.getenv("CACHE_VERSION", 0)
  })

  diagnoses_lkup <- shiny::reactive({
    readr::read_csv(app_sys("app", "data", "diagnoses.csv"), col_types = "ccc")
  })

  procedures_lkup <- shiny::reactive({
    readr::read_csv(app_sys("app", "data", "procedures.csv"), col_types = "ccc")
  })

  mitigator_codes_lkup <- shiny::reactive({
    lkup <- app_sys("app", "data", "mitigator-codes.csv") |>
      readr::read_csv(col_types = "c")

    purrr::set_names(
      paste0(lkup[["strategy_name"]], " (", lkup[["mitigator_code"]], ")"),
      lkup[["strategy"]]
    )
  })

  providers <- shiny::reactive({
    app_sys("app", "data", "providers.csv") |>
      readr::read_csv(col_types = "cc") |>
      tibble::deframe() # convert tibble to named vector
  })

  peers <- shiny::reactive({
    readr::read_csv(app_sys("app", "data", "peers.csv"), col_types = "cc")
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

  # load all data
  rates_data <- shiny::reactive({
    load_provider_data("rates")
  }) |>
    shiny::bindCache(cache_version())

  age_sex_data <- shiny::reactive({
    age_sex <- load_provider_data("age_sex")
    # nolint start: object_usage_linter
    age_fct <- age_sex |> _[["age_group"]] |> unique() |> sort()
    # nolint end
    age_sex |>
      dplyr::mutate(
        age_group = factor(
          .data[["age_group"]],
          levels = .env[["age_fct"]]
        )
      )
  }) |>
    shiny::bindCache(cache_version())

  diagnoses_data <- shiny::reactive({
    load_provider_data("diagnoses")
  }) |>
    shiny::bindCache(cache_version())

  procedures_data <- shiny::reactive({
    load_provider_data("procedures")
  }) |>
    shiny::bindCache(cache_version())

  baseline_data <- shiny::reactive({
    load_provider_data("baseline")
  }) |>
    shiny::bindCache(cache_version())

  covid_adjustment_data <- shiny::reactive({
    load_provider_data("covid_adjustment")
  }) |>
    shiny::bindCache(cache_version())

  wli_data <- shiny::reactive({
    load_provider_data("wli")
  }) |>
    shiny::bindCache(cache_version())

  expat_data <- shiny::reactive({
    load_provider_data("expat")
  }) |>
    shiny::bindCache(cache_version())

  repat_local_data <- shiny::reactive({
    load_provider_data("repat_local")
  }) |>
    shiny::bindCache(cache_version())

  repat_nonlocal_data <- shiny::reactive({
    load_provider_data("repat_nonlocal")
  }) |>
    shiny::bindCache(cache_version())

  params_schema <- shiny::reactive({
    get_params_schema()
  }) |>
    shiny::bindCache(cache_version())

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

    mod_covid_adjustment_server(
      "covid_adjustment",
      covid_adjustment_data(),
      params
    )

    mod_population_growth_server("population_growth", params)

    mod_health_status_adjustment_server("health_status_adjustment", params)

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
      params,
      providers()
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
      available_strategies,
      diagnoses_lkup(),
      procedures_lkup(),
      mitigator_codes_lkup(),
      peers()
    )

    # enable covid adjusment tab for 2019/20 baseline only
    output$tab_covid_adjustment <- bs4Dash::renderMenu({
      if (params$start_year == "201920") {
        bs4Dash::menuItem(
          "Covid Adjustment",
          tabName = "tab_covid_adjustment"
        )
      }
    })

    # enable the run_model page for certain users/running locally

    can_run_model <- any(
      c("nhp_devs", "nhp_power_users", "nhp_run_model") %in% session$groups
    )
    if (is_local() || can_run_model) {
      shinyjs::show("run-model-container")
      mod_run_model_server("run_model", params, params_schema())
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
      mod_run_model_fix_params(params_schema()) |>
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
