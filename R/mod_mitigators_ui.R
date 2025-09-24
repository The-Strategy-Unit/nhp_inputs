#' mitigators_admission_avoidance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mitigators_ui <- function(id, title, show_diagnoses_table = TRUE) {
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
            shiny::p(
              "Note that 100% means no change and 0% means all activity
                     mitigated"
            ),
            shiny::plotOutput(ns("nee_result"), height = 80),
            shiny::sliderInput(
              ns("slider"),
              "80% prediction interval",
              0,
              1,
              c(0, 1)
            ),
            shiny::htmlOutput(ns("slider_absolute")),
            shiny::p(),
            shiny::p(
              "Adjusting this slider will change the width of the
                     corresponding yellow-highlighted region in the trend, funnel
                     and boxplot charts above."
            )
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
            htmltools::HTML(
              paste0(
                "<details><summary>About</summary>",
                md_file_to_html("app", "text", "plot_trend.md"),
                "</details>"
              )
            ),
            width = 4
          ),
          bs4Dash::box(
            title = "Funnel",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("funnel_plot"))
            }),
            htmltools::HTML(
              paste0(
                "<details><summary>About</summary>",
                md_file_to_html("app", "text", "plot_funnel.md"),
                "</details>"
              )
            ),
            width = 4
          ),
          bs4Dash::box(
            title = "Boxplot",
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("boxplot"))
            }),
            htmltools::HTML(
              paste0(
                "<details><summary>About</summary>",
                md_file_to_html("app", "text", "plot_box.md"),
                "</details>"
              )
            ),
            width = 4
          ),
          col_6(
            bs4Dash::box(
              title = "Top 6 Primary Diagnoses",
              if (show_diagnoses_table) {
                shinycssloaders::withSpinner({
                  gt::gt_output(ns("diagnoses_table"))
                })
              } else {
                shiny::p("No diagnosis data for outpatients.")
              },
              width = 12
            ),
            bs4Dash::box(
              title = "Breakdown by Procedure",
              shinycssloaders::withSpinner({
                gt::gt_output(ns("procedures_table"))
              }),
              width = 12
            )
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
