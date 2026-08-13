#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  # handle loading the provided filename

  file <- parse_url_query_filename(request$QUERY_STRING)

  if (is.null(file)) {
    # redirect back to the inputs selection tool
    return(
      shiny::httpResponse(
        302L,
        headers = list(
          Location = get_golem_config("inputs_selection_app") %||%
            "http://localhost:9080/"
        )
      )
    )
  }
  dataset <- jsonlite::read_json(file.path(file))$dataset

  body <- list(
    bslib::nav_panel(
      "Home",
      icon = shiny::icon("house"),
      mod_home_ui("home")
    ),
    bslib::nav_menu(
      "Population Changes",
      icon = shiny::icon("user"),
      bslib::nav_panel(
        "Baseline Adjustment",
        mod_baseline_adjustment_ui("baseline_adjustment")
      ),
      bslib::nav_panel(
        "Population Growth",
        mod_population_growth_ui("population_growth", dataset)
      ),
      bslib::nav_panel(
        "Health Status Adjustment",
        mod_health_status_adjustment_ui("health_status_adjustment")
      ),
      bslib::nav_panel(
        "Non-demographic Adjustment",
        mod_non_demographic_adjustment_ui("non_demographic_adjustment")
      )
    ),
    bslib::nav_menu(
      "Demand-supply Imbalances",
      icon = shiny::icon("balance-scale"),
      bslib::nav_panel(
        "Inequalities",
        mod_inequalities_ui("inequalities")
      ),
      bslib::nav_panel(
        "Expat/Repat",
        mod_expat_repat_ui("expat_repat")
      )
    ),
    bslib::nav_menu(
      "Activity Mitigation",
      icon = shiny::icon("hand-holding-medical"),
      bslib::nav_panel(
        "Summary Totals",
        mod_mitigators_summary_ui("mitigators_summary")
      ),
      bslib::nav_item(
        shiny::tags$hr(),
        shiny::tags$h4("Inpatients", class = "dropdown-header")
      ),
      bslib::nav_panel(
        "Admission Avoidance",
        mod_mitigators_ui(
          "mitigators_admission_avoidance",
          "Admission Avoidance"
        )
      ),
      bslib::nav_panel(
        "Mean Length of Stay Reduction",
        mod_mitigators_ui(
          "mitigators_mean_los_reduction",
          "Mean Length of Stay Reduction"
        )
      ),
      bslib::nav_panel(
        "SDEC conversion",
        mod_mitigators_ui("mitigators_sdec_conversion", "SDEC conversion")
      ),
      bslib::nav_panel(
        "Pre-op Length of Stay Reduction",
        mod_mitigators_ui(
          "mitigators_preop_los_reduction",
          "Pre-op Length of Stay Reduction"
        )
      ),
      bslib::nav_panel(
        "Day Procedures: Daycase",
        mod_mitigators_ui(
          "mitigators_day_procedures_daycase",
          "Day Procedures: Daycase"
        )
      ),
      bslib::nav_panel(
        "Day Procedures: Outpatients",
        mod_mitigators_ui(
          "mitigators_day_procedures_outpatients",
          "Day Procedures: Outpatients"
        )
      ),
      bslib::nav_item(
        shiny::tags$hr(),
        shiny::tags$h4("Outpatients", class = "dropdown-header")
      ),
      bslib::nav_panel(
        "Consultant to Consultant Reduction",
        mod_mitigators_ui(
          "mitigators_op_c2c_reduction",
          "Consultant to Consultant Reduction",
          show_diagnoses_table = FALSE
        )
      ),
      bslib::nav_panel(
        "Convert to Tele Appointment",
        mod_mitigators_ui(
          "mitigators_op_convert_tele",
          "Convert to Tele Appointment",
          show_diagnoses_table = FALSE
        )
      ),
      bslib::nav_panel(
        "Follow-Up Reduction",
        mod_mitigators_ui(
          "mitigators_op_fup_reduction",
          "Follow-Up Reduction",
          show_diagnoses_table = FALSE
        )
      ),
      bslib::nav_panel(
        "GP Referred First Attendances",
        mod_mitigators_ui(
          "mitigators_op_gp_referred_first_attendance_reduction",
          "GP Referred First Attendances",
          show_diagnoses_table = FALSE
        )
      ),
      bslib::nav_item(
        shiny::tags$hr(),
        shiny::tags$h4("A&E", class = "dropdown-header")
      ),
      bslib::nav_panel(
        "Discharged with No Investigations or Treatments",
        mod_mitigators_ui(
          "mitigators_aae_discharged_no_treatment",
          "Discharged with No Investigations or Treatments"
        )
      ),
      bslib::nav_panel(
        "Frequent Attenders",
        mod_mitigators_ui(
          "mitigators_aae_frequent_attenders",
          "Frequent Attenders"
        )
      ),
      bslib::nav_panel(
        "Left Before Seen",
        mod_mitigators_ui("mitigators_aae_left_before_seen", "Left Before Seen")
      ),
      bslib::nav_panel(
        "Low Cost Discharged",
        mod_mitigators_ui(
          "mitigators_aae_low_cost_discharged",
          "Low Cost Discharged"
        )
      )
    ),
    bslib::nav_panel(
      "Run Model",
      icon = shiny::icon("play"),
      mod_run_model_ui("run_model")
    )
  )

  shiny::tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    bslib::page_navbar(
      title = "NHP Model Inputs",
      navbar_options = bslib::navbar_options(
        class = "bg-primary",
        theme = "dark"
      ),
      theme = ui_theme(),
      !!!body
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "NHP: Inputs"
    ),
    use_leafletjs(),
    tags$base(target = "_blank")
  )
}


ui_theme <- function() {
  brand <- brand.yml::read_brand_yml(app_sys("_brand.yml"))

  bslib::bs_theme(brand = brand) |>
    bslib::bs_add_rules(
      sass::sass_file(app_sys("app/www/styles.scss"))
    )
}
