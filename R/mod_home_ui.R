#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  # each of the columns is created in it's own variable

  # left column contains the documentation for this module
  left_column <- col_4(
    bs4Dash::box(
      collapsible = FALSE,
      headerBorder = FALSE,
      width = 12,
      md_file_to_html("app", "text", "home.md")
    )
  )

  # middle column contains the inputs that the user is going to set
  middle_column <- col_4(
    bs4Dash::box(
      title = "Select Provider and Baseline",
      collapsible = FALSE,
      width = 12,
      shiny::selectInput(ns("cohort"), "Cohort", c("Current", "All Other Providers")),
      shiny::selectInput(ns("dataset"), "Provider", choices = NULL, selectize = TRUE),
      shiny::selectInput(ns("start_year"), "Baseline Year", choices = c("2019" = 201920, "2018" = 201819)),
      shiny::sliderInput(ns("end_year"), "Model Year", min = 0, max = 19, value = 0, sep = "")
    ),
    bs4Dash::box(
      title = "Scenario",
      collapsible = FALSE,
      width = 12,
      shinyjs::disabled(
        shiny::radioButtons(
          ns("scenario_type"),
          NULL,
          c(
            "Create new from scratch",
            "Create new from existing",
            "Edit existing"
          ),
          inline = TRUE
        )
      ),
      shinyjs::hidden(
        shiny::selectInput(
          ns("previous_scenario"),
          "Previous Scenario",
          NULL
        )
      ),
      shiny::textInput(ns("scenario"), "Name"),
      shiny::textOutput(ns("status")),
      shinyjs::disabled(
        shiny::actionButton(ns("start"), "Start")
      )
    ),
    bs4Dash::box(
      title = "Advanced Options",
      width = 12,
      collapsed = TRUE,
      shiny::numericInput(ns("seed"), "Seed", sample(1:100000, 1)),
      shiny::selectInput(ns("model_runs"), "Model Runs", choices = c(256, 512, 1024), selected = 256)
    ),
    bs4Dash::box(
      title = "Upload Previous Set of Parameters",
      width = 12,
      collapsed = TRUE,
      shiny::fileInput(ns("param_upload"), "Upload")
    )
  )

  # right column contains the outputs in the home module (map and peers list)
  right_column <- col_4(
    bs4Dash::box(
      title = "Map of Selected Provider and Peers",
      width = 12,
      shinycssloaders::withSpinner(
        leaflet::leafletOutput(ns("providers_map"), height = "730px")
      )
    ),
    bs4Dash::box(
      title = "Peers (from NHS Trust Peer Finder Tool)",
      width = 12,
      collapsed = TRUE,
      shinycssloaders::withSpinner(
        gt::gt_output(ns("peers_list"))
      )
    )
  )

  # build the home page outputs
  shiny::tagList(
    htmltools::h1("NHP Model Inputs"),
    shiny::fluidRow(
      left_column,
      middle_column,
      right_column
    )
  )
}
