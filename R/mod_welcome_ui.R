#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)

  # TODO: MD
  bs4Dash::box(
    collapsible = FALSE,
    headerBorder = FALSE,
    width = 12,
    color = "gray-dark",
    list(
      htmltools::h1(strong("The New Hospital Programme (NHP)")),
      htmltools::h2("Demand and capacity model 2023/24: inputs stage"),
      actionButton("button_begin", "Begin")
    )
  )

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = TRUE,
        title = "Directions",
        width = 12,
        list(
          p("Welcome to this NHP online tool. This tool is designed to help facilitate a demand and capacity modelling process to support the development of robust, local NHP proposals."),
          p("The New Hospital Programme requires estimates of future activity levels to inform the design of a new hospital. This tool is designed to help determine how hospital activity might change in the years to come (relative to a baseline year) and to provide a high-level view of the physical capacity required to meet that demand.")
        )
      )
    ),
    shiny::column(
      width = 6,
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = TRUE,
        title = "Contact",
        width = 12,
        htmltools::HTML("For more information or help, please contact the <a href='mailto:mlcsu.nhpanalytics@nhs.net'>MLCSU NHP Analytics mailbox</a>.")
      )
    )
  )

}
