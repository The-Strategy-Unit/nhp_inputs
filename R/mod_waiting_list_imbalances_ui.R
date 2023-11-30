#' wli UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_waiting_list_imbalances_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$h1("Waiting List Imbalances"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          title = "Include in model",
          shinyWidgets::switchInput(
            ns("use_wli"),
            value = FALSE,
            onLabel = "Yes",
            offLabel = "No"
          )
        ),
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "waiting_list_imbalances.md")
        ),
        mod_reasons_ui(ns("reasons")),
        mod_time_profile_ui(ns("time_profile"))
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 8,
        gt::gt_output(ns("table"))
      )
    )
  )
}
