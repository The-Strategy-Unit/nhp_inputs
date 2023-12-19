#' expat_repat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expat_repat_ui <- function(id) {
  ns <- shiny::NS(id)

  generate_param_controls <- function(type, min, max, values) {
    shiny::fluidRow(
      col_3(
        shiny::checkboxInput(ns(glue::glue("include_{type}")), "Include?")
      ),
      col_9(
        shinyjs::disabled(
          shiny::sliderInput(
            ns(type),
            "Confidence Interval",
            min, max, values, 0.1,
            post = "%"
          )
        )
      )
    )
  }

  shiny::tagList(
    shiny::tags$h1("Expatriation/Repatriation"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          title = "Selection",
          width = 12,
          shiny::selectInput(
            ns("activity_type"),
            "Activity Type",
            c(
              "Inpatients" = "ip",
              "Outpatients" = "op",
              "A&E" = "aae"
            )
          ),
          shinyjs::hidden(
            shiny::selectInput(
              ns("ip_subgroup"),
              "Subgroup",
              c(
                "Elective" = "elective",
                "Non-Elective" = "non-elective",
                "Maternity" = "maternity"
              )
            )
          ),
          shiny::selectInput(
            ns("type"),
            NULL,
            NULL
          )
        ),
        mod_reasons_ui(ns("reasons")),
        mod_time_profile_ui(ns("time_profile")),
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "expat_repat.md")
        ),
      ),
      col_8(
        bs4Dash::box(
          title = "Expatriation Model Parameter",
          width = 12,
          generate_param_controls("expat", 0, 100, c(95, 100))
        ),
        bs4Dash::box(
          title = "Repatriation (Local) Model Parameter",
          width = 12,
          generate_param_controls("repat_local", 100, 500, c(100, 105)),
          shiny::fluidRow(
            col_6(
              shiny::plotOutput(
                ns("repat_local_plot"),
              )
            ),
            col_6(
              shiny::plotOutput(
                ns("repat_local_split_plot")
              )
            )
          )
        ),
        bs4Dash::box(
          title = "Repatriation (Non-Local) Model Parameter",
          width = 12,
          generate_param_controls("repat_nonlocal", 100, 500, c(100, 105)),
          shiny::fluidRow(
            col_4(
              shiny::plotOutput(
                ns("repat_nonlocal_pcnt_plot")
              )
            ),
            col_4(
              shiny::plotOutput(
                ns("repat_nonlocal_n")
              )
            ),
            col_4(
              leaflet::leafletOutput(
                ns("repat_nonlocal_icb_map")
              )
            )
          )
        )
      )
    )
  )
}
