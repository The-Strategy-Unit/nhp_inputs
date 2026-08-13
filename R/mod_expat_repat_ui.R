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
    bslib::layout_columns(
      col_widths = c(3, 9),
      shiny::checkboxInput(ns(glue::glue("include_{type}")), "Include?"),
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
  }

  shiny::tagList(
    shiny::tags$h1("Expatriation/Repatriation"),
    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::tagList(
        bslib::card(
          bslib::card_header(
            "Selection",
            class = "bg-primary"
          ),
          fill = FALSE,
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
        bslib::card(
          fill = FALSE,
          md_file_to_html("app", "text", "expat_repat.md")
        ),
      ),
      shiny::tagList(
        bslib::card(
          bslib::card_header(
            "Expatriation Model Parameter",
            class = "bg-primary"
          ),
          fill = FALSE,
          generate_param_controls("expat", 0, 100, c(95, 100))
        ),
        bslib::card(
          bslib::card_header(
            "Repatriation (Local) Model Parameter",
            class = "bg-primary"
          ),
          fill = FALSE,
          generate_param_controls("repat_local", 100, 500, c(100, 105)),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                ns("repat_local_plot"),
              )
            ),
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                ns("repat_local_split_plot")
              )
            )
          )
        ),
        bslib::card(
          bslib::card_header(
            "Repatriation (Non-Local) Model Parameter",
            class = "bg-primary"
          ),
          fill = FALSE,
          generate_param_controls("repat_nonlocal", 100, 500, c(100, 105)),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                ns("repat_nonlocal_pcnt_plot")
              )
            ),
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                ns("repat_nonlocal_n")
              )
            ),
            shiny::tags$div(
              id = ns("icb_map"),
              style = "height: 400px;"
            )
          )
        )
      )
    )
  )
}
