#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  header <- bs4Dash::dashboardHeader(title = "NHP Model Inputs")

  sidebar <- bs4Dash::dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    bs4Dash::sidebarMenu(
      id = "sidebarMenu",
      bs4Dash::menuItem(
        "Home",
        tabName = "tab_home",
        icon = shiny::icon("house")
      ),
      bs4Dash::menuItem(
        "Health Status Adjustment",
        tabName = "tab_hsa"
      ),
      bs4Dash::menuItem(
        "Population Growth",
        tabName = "tab_population_growth"
      ),
      bs4Dash::menuItem(
        "Non-demographic Adjustment",
        tabName = "tab_nda"
      ),
      bs4Dash::menuItem(
        "Waiting List Imbalances",
        tabName = "tab_wli"
      ),
      bs4Dash::menuItem(
        "Expat/Repat",
        tabName = "tab_er"
      ),
      bs4Dash::sidebarHeader("Activity Mitigators"),
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
          "AEC LoS Reduction",
          tabName = "ip_am_aec_los_reduction"
        ),
        bs4Dash::menuSubItem(
          "Pre-op LoS Reduction",
          tabName = "ip_am_preop_los_reduction"
        ),
        bs4Dash::menuSubItem(
          "Day Surgery",
          tabName = "ip_am_bads"
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
        )
      ),
      bs4Dash::menuItem(
        "A&E",
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
      bs4Dash::sidebarHeader("Debug"),
      bs4Dash::menuItem(
        "Show Params",
        tabName = "debug_params"
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
        tabName = "tab_hsa",
        mod_hsa_ui("hsa")
      ),
      bs4Dash::tabItem(
        tabName = "tab_population_growth",
        mod_population_growth_ui("population_growth")
      ),
      bs4Dash::tabItem(
        tabName = "tab_nda",
        mod_nda_ui("nda")
      ),
      bs4Dash::tabItem(
        tabName = "tab_wli",
        mod_wli_ui("wli")
      ),
      bs4Dash::tabItem(
        tabName = "tab_er",
        mod_expat_repat_ui("expat_repat")
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_admission_avoidance",
        mod_mitigators_ui("mitigators_admission_avoidance", "Admission Avoidance")
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_mean_los_reduction",
        mod_mitigators_ui("mitigators_mean_los_reduction", "Mean Length of Stay Reduction")
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_aec_los_reduction",
        mod_mitigators_ui("mitigators_aec_los_reduction", "AEC Length of Stay Reduction")
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_preop_los_reduction",
        mod_mitigators_ui("mitigators_preop_los_reduction", "Pre-op Length of Stay Reduction")
      ),
      bs4Dash::tabItem(
        tabName = "ip_am_bads",
        mod_mitigators_ui("mitigators_bads", "Day Surgery Type Conversion")
      ),
      bs4Dash::tabItem(
        tabName = "op_am_c2c_referrals",
        mod_mitigators_ui("mitigators_op_c2c_reduction", "Consultant to Consultant Reduction")
      ),
      bs4Dash::tabItem(
        tabName = "op_am_convert_tele",
        mod_mitigators_ui("mitigators_op_convert_tele", "Convert to Tele Appointment")
      ),
      bs4Dash::tabItem(
        tabName = "op_am_fup_reduction",
        mod_mitigators_ui("mitigators_op_fup_reduction", "Follow-Up Reduction")
      ),
      bs4Dash::tabItem(
        tabName = "aae_frequent_attenders",
        mod_mitigators_ui("mitigators_aae_frequent_attenders", "Frequent Attenders")
      ),
      bs4Dash::tabItem(
        tabName = "aae_left_before_seen",
        mod_mitigators_ui("mitigators_aae_left_before_seen", "Left Before Seen")
      ),
      bs4Dash::tabItem(
        tabName = "aae_low_cost_discharged",
        mod_mitigators_ui("mitigators_aae_low_cost_discharged", "Low Cost Discharged")
      ),
      bs4Dash::tabItem(
        tabName = "debug_params",
        mod_debug_params_ui("debug_params")
      )
    )
  )

  shiny::tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    bs4Dash::dashboardPage(
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
      app_title = "nhp_inputs"
    )
  )
}
