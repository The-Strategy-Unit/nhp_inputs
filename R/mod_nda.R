age_bands <- function() {
  c(
    " 0- 4",
    " 5-14",
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
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(
      shiny::HTML("
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
      ")
    ),
    shiny::selectInput(
      ns("activity_type"),
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
        shiny::tags$div(
          class = "label-left",
          shiny::sliderInput(
            inputId = ns(stringr::str_remove_all(.x, " ")),
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
    slider_values <- c("non-elective", "elective", "maternity") |>
      purrr::set_names() |>
      purrr::map(
        \(...) {
          purrr::map(
            purrr::set_names(age_bands()),
            \(...) c(100, 120)
          )
        }
      ) |>
      (purrr::lift_dl(shiny::reactiveValues))()


    shiny::observe({
      params[["non-demographic_adjustment"]] <- slider_values |>
        shiny::reactiveValuesToList() |>
        purrr::map_depth(2, `/`, 100)
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
            stringr::str_remove_all(.x, " "),
            value = slider_values[[at]][[.x]]
          )
        }
      )
    }) |>
      shiny::bindEvent(input$activity_type)

    # when a slider changes, update the values in params
    purrr::walk(
      age_bands(),
      \(.x) {
        i <- stringr::str_remove_all(.x, " ")
        shiny::observe({
          at <- shiny::req(input$activity_type)

          slider_values[[at]][[.x]] <- shiny::req(input[[i]])
          params[["non-demographic_adjustment"]][[at]][[.x]] <- slider_values[[at]][[.x]] / 100
        }) |>
          shiny::bindEvent(input[[i]])
      }
    )

    output$tmp <- shiny::renderPrint({
      slider_values |>
        shiny::reactiveValuesToList() |>
        jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
    })
  })
}
