#' mitigator_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mitigators_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header(
        "Type of Potentially Mitigatable Activity (TPMA)",
        class = "bg-primary"
      ),
      fill = FALSE,
      shiny::p(
        "This table summarises the most common healthcare activities
                across inpatients, outpatients, and A&E, to help prioritise
                the setting of TPMAs. Some forms of activity are
                uncommon and so setting a mitigating factor will have
                relatively little influence over the model results"
      )
    ),

    bslib::card(
      fill = FALSE,
      shinycssloaders::withSpinner(
        shiny::htmlOutput(ns("diagnoses_table"))
      )
    )
  )
}
