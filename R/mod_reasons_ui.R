#' reasons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reasons_ui <- function(id) {
  ns <- shiny::NS(id)
  bs4Dash::box(
    title = "Supporting Rationale",
    width = 12,
    shiny::textAreaInput(
      ns("value"),
      NULL,
      height = "200px",
      placeholder = paste(
        "Type your rationale here and it will be saved automatically.",
        "You can supply rationale even if you do not make a selection or adjustment."
      )
    )
  )
}
