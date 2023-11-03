#' bed_occupancy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bed_occupancy_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    col_4(
      shiny::fluidRow(
        bs4Dash::box(
          title = "Ward Groups",
          width = 12,
          shiny::selectInput(
            ns("ward_group"),
            "Ward Group",
            choices = "Other"
          ),
          shiny::sliderInput(
            ns("occupancy"),
            "Future Bed Occupancy",
            min = 0,
            max = 100,
            value = c(85, 90),
            step = 0.1,
            round = TRUE,
            post = "%"
          ),
          shiny::tags$hr(),
          shiny::fluidRow(
            col_6(
              bs4Dash::actionButton(
                ns("show_ward_group_modal"),
                "Add Group",
                icon = shiny::icon("add"),
                width = "100%",
                status = "success"
              )
            ),
            col_6(
              bs4Dash::actionButton(
                ns("remove_ward_group"),
                "Remove Group",
                icon = shiny::icon("remove"),
                width = "100%",
                status = "danger"
              )
            )
          )
        ),
        mod_reasons_ui(ns("reasons")),
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "bed_occupancy.md")
        )
      )
    ),
    col_8(
      shiny::tagList(
        purrr::pmap(
          dplyr::group_nest(mod_bed_occupancy_specialties(), .data[["group"]]),
          \(group, data) {
            bs4Dash::box(
              title = glue::glue("Specialty Mapping: {group}"),
              width = 12,
              mod_bed_occupancy_specialty_table(data, ns)
            )
          }
        )
      )
    )
  )
}
