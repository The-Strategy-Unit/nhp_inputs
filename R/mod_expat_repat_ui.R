#' expat_repat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#'
#' @noRd
mod_expat_repat_ui <- function(id) {
  ns <- shiny::NS(id)

  # Generate parameter controls
  #
  # Internal helper function to create UI controls for expatriation/repatriation
  # parameters including a checkbox and slider.
  #
  # @param type Parameter type identifier (e.g., "expat", "repat_local").
  # @param min Minimum value for the slider.
  # @param max Maximum value for the slider.
  # @param values Initial values for the slider range.
  #
  # @return A shiny fluidRow containing the parameter controls.
  # @noRd
  generate_param_controls <- function(type, min, max, values) {
    shiny::fluidRow(
      col_3(
        shiny::checkboxInput(ns(glue::glue("include_{type}")), "Include?")
      ),
      col_9(
        shinyjs::disabled(
          shiny::sliderInput(
            ns(type),
            "Prediction interval",
            min,
            max,
            values,
            0.1,
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
