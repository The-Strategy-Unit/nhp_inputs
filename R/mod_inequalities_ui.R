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

  correction_options <- c(
    "No change" = "no_change",
    "Zero sum" = "zero_sum",
    "Level down" = "level_down",
    "Level up" = "level_up"
  )

  plot_height <- 100

  shiny::tagList(
    shiny::tags$h1("Inequalities"),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::fluidRow(
          bs4Dash::box(
            width = 12,
            headerBorder = FALSE,
            collapsible = FALSE,
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi
            ornare dapibus mi vel commodo. Nulla et dui in elit dapibus posuere
            non quis velit. Donec at euismod ante, nec lacinia sem. Nam mattis
            aliquam arcu ac pretium. Vivamus luctus quis arcu non dictum. Ut
            eleifend neque eget massa pulvinar, at mollis enim lacinia. Donec
            placerat metus nec ipsum accumsan, ut imperdiet diam posuere.",
            shiny::p(),
            shiny::downloadButton(
              outputId = "btn_download",
              label = "Download data",
              icon = shiny::icon("download")
            )
          ),
          mod_reasons_ui(ns("reasons"))
        )
      ),
      shiny::column(
        width = 8,
        shiny::fluidRow(
          bs4Dash::accordion(
            id = "accordion",
            width = 12,
            bs4Dash::accordionItem(
              title = "Subchapter LB: Urological and Male Reproductive System Procedures and Disorders",
              collapsed = FALSE,
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  offset = 4,
                  shiny::actionButton(
                    inputId = "btn_lb",
                    label = "Set all to zero sum",
                    width = "100%"
                  )
                )
              ),
              shiny::p(),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  "LB42: Dynamic Studies of Urinary Tract",
                ),
                shiny::column(
                  width = 4,
                  shiny::selectInput(
                    inputId = "option_lb42",
                    label = NULL,
                    choices = correction_options,
                    selected = "no_change",
                    selectize = FALSE
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::plotOutput("plot_lb42", height = plot_height)
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  "LB72: Diagnostic Flexible Cystoscopy"
                ),
                shiny::column(
                  width = 4,
                  shiny::selectInput(
                    inputId = "option_lb72",
                    label = NULL,
                    choices = correction_options,
                    selected = "no_change",
                    selectize = FALSE
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::plotOutput("plot_lb72", height = plot_height)
                )
              )
            ),
            bs4Dash::accordionItem(
              title = "Subchapter X: Subchapter Name",
              collapsed = TRUE,
              "Another set of options."
            )
          )
        )
      )
    )
  )
}
