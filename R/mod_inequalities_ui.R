#' inequalities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_inequalities_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1("Inequalities"),
    shiny::fluidRow(
      col_6(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "inequalities.md"),
          bs4Dash::box(
            collapsible = FALSE,
            headerBorder = FALSE,
            width = 12,
            md_file_to_html("app", "text", "inequalities_cont.md")
          )
        )
      ),
      col_6(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          shiny::tags$img(src = "www/inequality.png", width = "100%")
        )
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 6,
        shiny::tags$p("Please select the appropriate change below"),
        shiny::selectInput(ns("change"), "",
          choices = c(
            "No change", "Level up", "Level down",
            "Zero sum redistribution"
          )
        )
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        shiny::tags$img(src = "www/level_up.png", width = "100%")
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        shiny::tags$img(src = "www/level_down.png", width = "100%")
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 4,
        shiny::tags$img(src = "www/zero_sum.png", width = "100%")
      )
    )
  )
}
