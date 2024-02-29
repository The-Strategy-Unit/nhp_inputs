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

  left_column <- col_4(
    bs4Dash::box(
      collapsible = FALSE,
      headerBorder = FALSE,
      width = 12,
      md_file_to_html("app", "text", "home.md")
    )
  )

  right_column <- col_8(
    bs4Dash::box(
      title = "Model Options",
      collapsible = FALSE,
      width = 12,
      shinycssloaders::withSpinner(
        gt::gt_output(ns("model_options"))
      )
    )
  )

  # build the home page outputs
  shiny::tagList(
    htmltools::h1("NHP Model Inputs"),
    shiny::fluidRow(
      left_column,
      right_column
    )
  )
}
