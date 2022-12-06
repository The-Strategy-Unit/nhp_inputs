age_bands <- function() {
  c(
    "00-04",
    "05-14",
    "15-34",
    "35-49",
    "50-64",
    "65-84",
    "85+"
  )
}

#' nda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nda_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    tags$style(HTML(
      "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
    )),
    shiny::selectInput(ns("activity_type"),
      label = "Activity Type",
      choices = c(
        "Non-Elective",
        "Elective",
        "Maternity"
      ) |>
        purrr::set_names() |>
        purrr::map_chr(stringr::str_to_lower)
    ),
    purrr::map(
      age_bands(),
      \(.x) {
        div(
          class = "label-left",
          shiny::sliderInput(
            inputId = ns(.x),
            label = .x,
            min = 0,
            max = 200,
            post = "%",
            value = c(50, 150)
          )
        )
      }
    ),
    bs4Dash::box(
      title = "debug",
      shiny::verbatimTextOutput(ns("tmp"), placeholder = TRUE)
    )
  )
}

#' nda Server Functions
#'
#' @noRd
mod_nda_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      params[["non-demographic_adjustment"]] <- purrr::map(
        purrr::set_names(c("non-elective", "elective", "maternity")),
        \(...) {
          purrr::map(
            purrr::set_names(age_bands()),
            \(...) c(1, 1.2)
          )
        }
      )
    })

    # when the activity_type input changes, update all of the sliders
    # to use the values stored in params
    shiny::observe({
      at <- shiny::req(input$activity_type)

      purrr::walk(
        age_bands(),
        \(.x) {
          shiny::updateSliderInput(
            session,
            .x,
            value = params[["non-demographic_adjustment"]][[at]][[.x]] * 100
          )
        }
      )
    }) |>
      shiny::bindEvent(input$activity_type)

    # when a slider changes, update the values in params
    purrr::walk(
      age_bands(),
      \(.x) {
        shiny::observe({
          at <- shiny::req(input$activity_type)

          params[["non-demographic_adjustment"]][[at]][[.x]] <- shiny::req(input[[.x]]) / 100
        }) |>
          shiny::bindEvent(input[[.x]])
      }
    )

    output$tmp <- shiny::renderPrint({
      params[["non-demographic_adjustment"]] |>
        jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
    })
  })
}

## To be copied in the UI
# mod_nda_ui("nda_1")

## To be copied in the server
# mod_nda_server("nda_1")
