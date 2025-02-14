#' mitigator_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mitigators_summary_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      col_4(
        bs4Dash::box(
          title = "Activity Mitigator",
          width = 12,
          shiny::p("This table summarises the most common healthcare activities
                   across inpatients, outpatients, and A&E, to help prioritise
                   the setting of mitigators. Some forms of activity are
                   uncommon and so setting a mitigating factor will have
                   relatively little influence over the model results")
        )
      ),

      col_4(
        bs4Dash::box(
          width = 12,
          shinycssloaders::withSpinner(
            gt::gt_output(ns("diagnoses_table"))
          )
        )
      )
    )
  )
}

