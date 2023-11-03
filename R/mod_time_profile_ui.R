#' time_profile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_time_profile_ui <- function(id) {
  ns <- shiny::NS(id)
  bs4Dash::bs4Card(
    title = "Time Profile",
    width = 12,
    shiny::selectInput(
      ns("time_profile"),
      "Time Profile",
      purrr::set_names(
        c("linear", "front_loaded", "back_loaded", "step_change"),
        snakecase::to_title_case
      )
    ),
    shinyjs::hidden(
      shiny::sliderInput(
        ns("step_year"),
        "Step Change Year",
        min = 0,
        max = 1,
        value = 1,
        step = 1,
        sep = ""
      )
    )
  )
}
