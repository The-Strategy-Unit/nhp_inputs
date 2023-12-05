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

    gt::gt_output(ns("diagnoses_table"))

  )
}

