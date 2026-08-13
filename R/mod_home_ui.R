#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  left_column <- bslib::card(
    fill = FALSE,
    md_file_to_html("app", "text", "home.md")
  )

  right_column <- bslib::card(
    bslib::card_header(
      "Model Options",
      class = "bg-primary"
    ),
    fill = FALSE,
    shinycssloaders::withSpinner(
      shiny::htmlOutput(ns("model_options"))
    )
  )

  # build the home page outputs
  shiny::tagList(
    htmltools::h1("NHP Model Inputs"),
    bslib::layout_columns(
      col_widths = c(4, 8),
      left_column,
      right_column
    )
  )
}
