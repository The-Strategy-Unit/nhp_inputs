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
  bslib::card(
    bslib::card_header(
      "Supporting Rationale",
      class = "bg-primary"
    ),
    fill = FALSE,
    shiny::textAreaInput(
      ns("value"),
      NULL,
      height = "200px",
      width = "100%",
      placeholder = paste(
        "Type your rationale here and it will be saved automatically.",
        "You can supply rationale even if you do not make a selection or adjustment."
      )
    )
  )
}
