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
    shiny::h1("Types of Potentially Mitigatable Activity (TPMAs)"),
    shiny::h2(title),
    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::fluidRow(
        bslib::card(
          bslib::card_header(
            "Type of Potentially Mitigatable Activity",
            class = "bg-primary"
          ),
          fill = FALSE,
          shiny::selectInput(
            ns("strategy"),
            "Selection",
            choices = NULL,
            width = "100%"
          ),
          shiny::uiOutput(ns("strategy_text"))
        ),
        bslib::card(
          bslib::card_header(
            "Model Parameter",
            class = "bg-primary"
          ),
          fill = FALSE,
          shiny::checkboxInput(
            ns("include"),
            "Include?"
          ),
          shiny::p(
            "Note that 100% means no change and 0% means all activity mitigated"
          ),
          shiny::plotOutput(ns("nee_result"), height = 80),
          shiny::sliderInput(
            ns("slider"),
            "80% prediction interval",
            min = 0,
            max = 100,
            value = c(0, 100),
            step = 0.1,
            width = "100%"
          ),
          shiny::htmlOutput(ns("slider_absolute")),
          shiny::p(),
          shiny::htmlOutput(ns("slider_interval_text"))
        ),
        mod_reasons_ui(ns("reasons"))
      ),
      shiny::tagList(
        bslib::layout_columns(
          fill = FALSE,
          col_widths = c(5, 5, 2),
          bslib::card(
            bslib::card_header(
              "Trend",
              class = "bg-primary"
            ),
            fill = FALSE,
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("trend_plot"))
            })
          ),
          bslib::card(
            bslib::card_header(
              "Funnel",
              class = "bg-primary"
            ),
            fill = FALSE,
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("funnel_plot"))
            })
          ),
          bslib::card(
            bslib::card_header(
              "Boxplot",
              class = "bg-primary"
            ),
            fill = FALSE,
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("boxplot"))
            })
          )
        ),
        bslib::layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          shiny::tagList(
            bslib::card(
              bslib::card_header(
                "Top 6 Primary Diagnoses",
                class = "bg-primary"
              ),
              fill = FALSE,
              if (show_diagnoses_table) {
                shinycssloaders::withSpinner({
                  shiny::htmlOutput(ns("diagnoses_table"))
                })
              } else {
                shiny::p("No diagnosis data for outpatients.")
              }
            ),
            bslib::card(
              bslib::card_header(
                "Breakdown by Procedure",
                class = "bg-primary"
              ),
              fill = FALSE,
              shinycssloaders::withSpinner({
                shiny::htmlOutput(ns("procedures_table"))
              })
            )
          ),
          bslib::card(
            bslib::card_header(
              "Bar Chart of Activity by Age and Sex",
              class = "bg-primary"
            ),
            fill = FALSE,
            shinycssloaders::withSpinner({
              shiny::plotOutput(ns("age_grp_plot"))
            })
          )
        )
      )
    )
  )
}
