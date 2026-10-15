#' baseline_adjustment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_baseline_adjustment_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$h1("Baseline Adjustment"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "baseline_adjustment.md"),
          shinyjs::hidden(
            shiny::downloadButton(
              ns("download_baseline"),
              "Download Baseline Values (excel)"
            )
          )
        ),
        mod_reasons_ui(ns("reasons"))
      ),
      bs4Dash::box(
        title = "Parameters",
        width = 8,
        collapsible = FALSE,
        bs4Dash::tabsetPanel(
          shiny::tabPanel(
            "Inpatients",
            bs4Dash::tabsetPanel(
              shiny::tabPanel(
                "Elective",
                shiny::uiOutput(ns("ip_elective"))
              ),
              shiny::tabPanel(
                "Non-Elective",
                shiny::uiOutput(ns("ip_non-elective"))
              ),
              shiny::tabPanel(
                "Maternity",
                shiny::uiOutput(ns("ip_maternity"))
              )
            )
          ),
          shiny::tabPanel(
            "Outpatients",
            bs4Dash::tabsetPanel(
              shiny::tabPanel(
                "First Attendance",
                shiny::uiOutput(ns("op_first"))
              ),
              shiny::tabPanel(
                "Follow-up Attendance",
                shiny::uiOutput(ns("op_followup"))
              ),
              shiny::tabPanel(
                "Procedure",
                shiny::uiOutput(ns("op_procedure"))
              )
            )
          ),
          shiny::tabPanel(
            "A&E",
            bs4Dash::tabsetPanel(
              shiny::tabPanel("Walk-in", shiny::uiOutput(ns("aae_walk-in"))),
              shiny::tabPanel("Ambulance", shiny::uiOutput(ns("aae_ambulance")))
            )
          )
        )
      )
    )
  )
}
