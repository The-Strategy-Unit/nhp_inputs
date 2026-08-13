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
    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::tagList(
        bslib::card(
          bslib::card_header(
            "Include in model",
            class = "bg-primary"
          ),
          fill = FALSE,
          shinyWidgets::switchInput(
            ns("use_wli"),
            value = FALSE,
            onLabel = "Yes",
            offLabel = "No"
          )
        ),
        bslib::card(
          fill = FALSE,
          md_file_to_html("app", "text", "waiting_list_imbalances.md")
        ),
        mod_reasons_ui(ns("reasons"))
      ),
      bslib::card(
        fill = FALSE,
        shiny::htmlOutput(ns("table"))
      )
    )
  )
}
