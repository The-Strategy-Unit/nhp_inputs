#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  # handle loading the provided filename

  f <- utils::URLdecode(stringr::str_sub(request$QUERY_STRING, 2L))

  file <- file.path(get_golem_config("params_data_path"), "tmp", f)

  if (f == "" || !file.exists(file)) {
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
  dataset <- jsonlite::read_json(file)$dataset

  header <- bs4Dash::dashboardHeader(title = "NHP Model Inputs")

  sidebar <- bs4Dash::dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    bs4Dash::sidebarMenu(
      id = "sidebarMenu",
      #
      bs4Dash::menuItem(
        "Home",
        tabName = "tab_home",
        icon = shiny::icon("house")
      ),
      shiny::tags$hr(),
      bs4Dash::sidebarHeader("Baseline Adjustments"),
      bs4Dash::menuItem(
        "Baseline Adjustment",
        tabName = "tab_baseline_adjustment"
      ),
      bs4Dash::menuItemOutput("tab_covid_adjustment"), # only enable for 2019/20
      shiny::tags$hr(),
      bs4Dash::sidebarHeader("Population Changes"),
      bs4Dash::menuItem(
        "Population Growth",
        tabName = "tab_population_growth"
      ),
      bs4Dash::menuItem(
        "Health Status Adjustment",
        tabName = "tab_health_status_adjustment"
      ),
      #
      shiny::tags$hr(),
      bs4Dash::sidebarHeader("Demand-supply Imbalances"),
      bs4Dash::menuItem(
        "Expat/Repat",
        tabName = "tab_er"
      ),
      shiny::tags$hr(),
      bs4Dash::sidebarHeader("Non-demographic Changes"),
      bs4Dash::menuItem(
        "Non-demographic Adjustment",
        tabName = "tab_nda"
      ),
      #
      shiny::tags$hr(),
      bs4Dash::sidebarHeader("Activity Mitigators"),
      bs4Dash::menuItem(
        "Summary totals",
        tabName = "mitigators_summary"
      ),
      bs4Dash::menuItem(
        "Inpatients",
        bs4Dash::menuSubItem(
          "Admission Avoidance",
          tabName = "ip_am_admission_avoidance"
        ),
        bs4Dash::menuSubItem(
          "Mean LoS Reduction",
          tabName = "ip_am_mean_los_reduction"
        ),
        bs4Dash::menuSubItem(
          "SDEC conversion",
          tabName = "ip_am_sdec_conversion"
        ),
        bs4Dash::menuSubItem(
          "Pre-op LoS Reduction",
          tabName = "ip_am_preop_los_reduction"
        ),
        bs4Dash::menuSubItem(
          "Day Procedures: Daycase",
          tabName = "ip_am_mitigators_day_procedures_daycase"
        ),
        bs4Dash::menuSubItem(
          "Day Procedures: OP",
          tabName = "ip_am_mitigators_day_procedures_outpatients"
        )
      ),
      bs4Dash::menuItem(
        "Outpatients",
        bs4Dash::menuSubItem(
          "Consultant Referrals",
          tabName = "op_am_c2c_referrals"
        ),
        bs4Dash::menuSubItem(
          "Convert to Tele",
          tabName = "op_am_convert_tele"
        ),
        bs4Dash::menuSubItem(
          "Followup Reduction",
          tabName = "op_am_fup_reduction"
        ),
        bs4Dash::menuSubItem(
          "GP Referred First Att.",
          tabName = "op_gp_referred_first_attendance_reduction"
        )
      ),
      bs4Dash::menuItem(
        "A&E",
        bs4Dash::menuSubItem(
          "Discharged No Treatment",
          tabName = "aae_discharged_no_treatment"
        ),
        bs4Dash::menuSubItem(
          "Frequent Attenders",
          tabName = "aae_frequent_attenders"
        ),
        bs4Dash::menuSubItem(
          "Left Before Seen",
          tabName = "aae_left_before_seen"
        ),
        bs4Dash::menuSubItem(
          "Low Cost Discharged",
          tabName = "aae_low_cost_discharged"
        )
      ),
      #
      shiny::tags$hr(),
      #
      shinyjs::hidden(
        shiny::tags$div(
          id = "run-model-container",
          shiny::tags$hr(),
          bs4Dash::sidebarHeader("Run Model"),
          bs4Dash::menuItem(
            "Run Model",
            tabName = "tab_run_model"
          )
        )
      )
    )
  )

  body <- bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "tab_home",
        mod_home_ui("home")
      ),
      bs4Dash::tabItem(
        tabName = "tab_baseline_adjustment",
        mod_baseline_adjustment_ui("baseline_adjustment")
      ),
      bs4Dash::tabItem(
        tabName = "tab_covid_adjustment",
        mod_covid_adjustment_ui("covid_adjustment")
      ),
      bs4Dash::tabItem(
        tabName = "tab_population_growth",
        mod_population_growth_ui("population_growth", dataset)
      ),
      bs4Dash::tabItem(
        tabName = "tab_health_status_adjustment",
        mod_health_status_adjustment_ui("health_status_adjustment")
      ),
      bs4Dash::tabItem(
        tabName = "tab_nda",
        mod_non_demographic_adjustment_ui("non_demographic_adjustment")
      ),
      bs4Dash::tabItem(
        tabName = "tab_wli",
        mod_waiting_list_imbalances_ui("waiting_list_imbalances")
      ),
      bs4Dash::tabItem(
        tabName = "tab_er",
        mod_expat_repat_ui("expat_repat")
      ),
      bs4Dash::tabItem(
        tabName = "mitigators_summary",
        mod_mitigators_summary_ui("mitigators_summary")
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_admission_avoidance",
        mod_mitigators_ui(
          "mitigators_admission_avoidance",
          "Admission Avoidance"
        )
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_mean_los_reduction",
        mod_mitigators_ui(
          "mitigators_mean_los_reduction",
          "Mean Length of Stay Reduction"
        )
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_sdec_conversion",
        mod_mitigators_ui("mitigators_sdec_conversion", "SDEC conversion")
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_preop_los_reduction",
        mod_mitigators_ui(
          "mitigators_preop_los_reduction",
          "Pre-op Length of Stay Reduction"
        )
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_mitigators_day_procedures_daycase",
        mod_mitigators_ui(
          "mitigators_day_procedures_daycase",
          "Day Procedures: Daycase"
        )
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_mitigators_day_procedures_outpatients",
        mod_mitigators_ui(
          "mitigators_day_procedures_outpatients",
          "Day Procedures: Outpatients"
        )
      ),
      bs4Dash::tabItem(
        tabName = "op_am_c2c_referrals",
        mod_mitigators_ui(
          "mitigators_op_c2c_reduction",
          "Consultant to Consultant Reduction",
          show_diagnoses_table = FALSE
        )
      ),
      bs4Dash::tabItem(
        tabName = "op_am_convert_tele",
        mod_mitigators_ui(
          "mitigators_op_convert_tele",
          "Convert to Tele Appointment",
          show_diagnoses_table = FALSE
        )
      ),
      bs4Dash::tabItem(
        tabName = "op_am_fup_reduction",
        mod_mitigators_ui(
          "mitigators_op_fup_reduction",
          "Follow-Up Reduction",
          show_diagnoses_table = FALSE
        )
      ),
      bs4Dash::tabItem(
        tabName = "op_gp_referred_first_attendance_reduction",
        mod_mitigators_ui(
          "mitigators_op_gp_referred_first_attendance_reduction",
          "GP Referred First Attendances",
          show_diagnoses_table = FALSE
        )
      ),
      bs4Dash::tabItem(
        tabName = "aae_discharged_no_treatment",
        mod_mitigators_ui(
          "mitigators_aae_discharged_no_treatment",
          "Discharged with No Investigations or Treatments"
        )
      ),
      bs4Dash::tabItem(
        tabName = "aae_frequent_attenders",
        mod_mitigators_ui(
          "mitigators_aae_frequent_attenders",
          "Frequent Attenders"
        )
      ),
      bs4Dash::tabItem(
        tabName = "aae_left_before_seen",
        mod_mitigators_ui("mitigators_aae_left_before_seen", "Left Before Seen")
      ),
      bs4Dash::tabItem(
        tabName = "aae_low_cost_discharged",
        mod_mitigators_ui(
          "mitigators_aae_low_cost_discharged",
          "Low Cost Discharged"
        )
      ),
      bs4Dash::tabItem(
        tabName = "tab_run_model",
        mod_run_model_ui("run_model")
      )
    )
  )

  shiny::tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    shiny::conditionalPanel(
      "false",
      shiny::textInput("params_file", NULL, file)
    ),
    bs4Dash::dashboardPage(
      help = NULL,
      dark = NULL,
      header,
      sidebar,
      body
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
    tags$base(target = "_blank")
  )
}
