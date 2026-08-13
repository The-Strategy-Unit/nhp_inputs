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
    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::tagList(
        bslib::card(
          fill = FALSE,
          md_file_to_html("app", "text", "non_demographic_adjustment.md")
        ),
        bslib::card(
          bslib::card_header(
            "Non-demographic Variant",
            class = "bg-primary"
          ),
          md_file_to_html(
            "app",
            "text",
            "non_demographic_adjustment_variants_pt1.md"
          ),
          shiny::selectInput(
            inputId = ns("ndg_variant"),
            label = "Selection",
            choices = purrr::set_names(
              c("variant_2", "variant_3"),
              snakecase::to_title_case
            ),
            selected = "variant_2"
          ),
          md_file_to_html(
            "app",
            "text",
            "non_demographic_adjustment_variants_pt2.md"
          )
        )
      ),
      bslib::card(
        fill = TRUE,
        shiny::htmlOutput(ns("non_demographic_adjustment_table"))
      )
    )
  )
}
