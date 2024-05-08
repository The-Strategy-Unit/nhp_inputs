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
  shiny::tagList(
    shiny::tags$h1("Non-demographic Adjustment"),
    shiny::fluidRow(
      bs4Dash::column(
        width = 4,
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "non_demographic_adjustment.md")
        ),
        shinyjs::hidden(
          shiny::tags$div(
            id = ns("ndg_variant_box"),
            bs4Dash::box(
              title = "Non-demographic Variant",
              md_file_to_html("app", "text", "non_demographic_adjustment_variants.md"),
              width = 12,
              shiny::selectInput(
                inputId = ns("ndg_variant"),
                label = "Selection",
                choices = purrr::set_names(
                  c("variant_1", "variant_2"),
                  snakecase::to_title_case
                ),
                selected = "variant_2"
              )
            ),
            mod_reasons_ui(ns("reasons"))
          )
        )
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        gt::gt_output(ns("non_demographic_adjustment_table"))
      )
    )
  )
}
#
