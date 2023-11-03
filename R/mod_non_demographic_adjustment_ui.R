#' Non Demographic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_non_demographic_adjustment_ui <- function(id) {
  ns <- shiny::NS(id)

  boxes <- purrr::map(
    nda_groups,
    \(.x) bs4Dash::box(
      collapsible = FALSE,
      headerBorder = FALSE,
      title = .x$name,
      width = 8,
      purrr::imap(
        .x$values,
        \(.x, .i) mod_non_demographic_adjustment_input_ui(id, .i, .x)
      )
    )
  )

  shiny::tagList(
    shiny::tags$h1("Non-demographic Adjustment"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "non_demographic_adjustment.md")
        ),
        mod_reasons_ui(ns("reasons")),
        mod_time_profile_ui(ns("time_profile"))
      ),
      col_8(boxes)
    )
  )
}
