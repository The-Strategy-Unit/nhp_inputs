#' mitigators_admission_avoidance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mitigators_ui <- function(id, title) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Activity Mitigators"),
    shiny::h2(title),
    shiny::fluidRow(
      col_4(
        shiny::fluidRow(
          bs4Dash::box(
            title = "Activity Mitigator",
            width = 12,
            shiny::selectInput(ns("strategy"), "Selection", choices = NULL),
            shiny::uiOutput(ns("strategy_text"))
          ),
          bs4Dash::box(
            title = "Model Parameter",
            width = 12,
            shiny::checkboxInput(
              ns("include"),
              "Include?"
            ),
            shiny::radioButtons(
              ns("slider_type"),
              "Display Type",
              choices = c("Relative" = "% change", "Absolute" = "rate"),
              selected = "% change"
            ),
            shiny::plotOutput(ns("nee_result"), height = 80),
            shiny::sliderInput(ns("slider"), "80% Confidence Interval", 0, 1, c(0, 1))
          ),
          mod_reasons_ui(ns("reasons")),
          mod_time_profile_ui(ns("time_profile")),
        )
      ),
      col_8(
        shiny::fluidRow(
          bs4Dash::box(
            title = "Trend",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("trend_plot"))
            }),
            width = 5
          ),
          bs4Dash::box(
            title = "Funnel",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("funnel_plot"))
            }),
            width = 5
          ),
          bs4Dash::box(
            title = "Boxplot",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("boxplot"))
            }),
            width = 2
          ),
          bs4Dash::box(
            title = "Top 6 Primary Diagnoses",
            shinycssloaders::withSpinner({
              gt::gt_output(ns("diagnoses_table"))
            }),
            width = 6
          ),
          bs4Dash::box(
            title = "Bar Chart of Activity by Age and Sex",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("age_grp_plot"))
            }),
            width = 6
          )
        )
      )
    )
  )
}
