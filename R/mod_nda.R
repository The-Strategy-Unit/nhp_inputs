age_bands <- function() {
  bands <- c(
    " 0- 4",
    " 5-14",
    "15-34",
    "35-49",
    "50-64",
    "65-84",
    "85+"
  )
  purrr::set_names(
    bands,
    paste0("ageband_", seq_along(bands))
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
    shiny::tags$h1("Non-demographic Adjustment"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          title = "Activity Type",
          width = 12,
          shiny::selectInput(
            ns("activity_type"),
            label = NULL,
            choices = c(
              "Non-Elective",
              "Elective",
              "Maternity"
            ) |>
              purrr::set_names() |>
              purrr::map_chr(stringr::str_to_lower)
          )
        ),
        mod_time_profile_ui(ns("time_profile")),
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "nda.md")
        )
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        title = "Age Adjustment",
        width = 8,
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
        purrr::imap(
          age_bands(),
          \(.x, .i) {
            shiny::fluidRow(
              col_9(
                shiny::tags$div(
                  class = "label-left",
                  shinyjs::disabled(
                    shiny::sliderInput(
                      inputId = ns(.i),
                      label = .x,
                      min = 0,
                      max = 200,
                      post = "%",
                      value = c(100, 120)
                    )
                  )
                )
              ),
              col_3(shiny::checkboxInput(
                ns(glue::glue("include_{.i}")),
                "Include?"
              ))
            )
          }
        )
      )
    )
  )
}

#' nda Server Functions
#'
#' @noRd
mod_nda_server <- function(id, params) { # nolint: object_usage_linter.
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  shiny::moduleServer(id, function(input, output, session) {
    slider_values <- c("non-elective", "elective", "maternity") |>
      purrr::set_names() |>
      purrr::map(
        \(...) {
          n <- names(age_bands())

          purrr::set_names(
            rep(list(c(100, 120)), length(n)),
            n
          )
        }
      ) |>
      (purrr::lift_dl(shiny::reactiveValues))()

    # when the activity_type input changes, update all of the sliders
    # to use the values stored in params
    shiny::observe({
      at <- shiny::req(input$activity_type)

      purrr::iwalk(
        age_bands(),
        \(.x, .i) {
          shiny::updateSliderInput(
            session,
            .i,
            value = slider_values[[at]][[.x]]
          )

          shiny::updateCheckboxInput(
            session,
            glue::glue("include_{.i}"),
            value = !is.null(params[["non-demographic_adjustment"]][[at]][[.x]])
          )
        }
      )
    }) |>
      shiny::bindEvent(input$activity_type)

    init <- shiny::observe({
      params[["non-demographic_adjustment"]] <- list(
        "non-elective" = list(),
        "elective" = list(),
        "maternity" = list()
      )
      init$destroy()
    })

    # update the time profile
    shiny::observe({
      params$time_profile_mappings[["non-demographic_adjustment"]] <- selected_time_profile()
    }) |>
      shiny::bindEvent(selected_time_profile())

    shiny::observe({
      shiny::req(session$userData$data_loaded())
      p <- shiny::req(session$userData$params[["non-demographic_adjustment"]])

      # update the selected time profile
      update_time_profile(session$userData$params$time_profile_mappings[["non-demographic_adjustment"]])

      shiny::updateSelectInput(session, "activity_type", selected = "non-elective")

      tidyr::expand_grid(
        activity_type = c("non-elective", "elective", "maternity"),
        age_band = tibble::enframe(age_bands(), "id", "age_band")
      ) |>
        tidyr::unnest_wider("age_band") |>
        purrr::pwalk(\(activity_type, id, age_band) {
          v <- p[[activity_type]][[age_band]]
          params[["non-demographic_adjustment"]][[activity_type]][[age_band]] <- v

          vv <- (v * 100) %||% c(100, 120)

          slider_values[[activity_type]][[age_band]] <- vv

          if (activity_type == "non-elective") {
            shiny::updateSliderInput(session, id, value = vv)
            shiny::updateCheckboxInput(session, glue::glue("include_{id}"), value = !is.null(v))
          }
        })
    }) |>
      shiny::bindEvent(session$userData$data_loaded())

    # when a slider changes, update the values in params
    purrr::iwalk(
      age_bands(),
      \(.x, .i) {
        include_type <- glue::glue("include_{.i}")

        shiny::observe({
          at <- shiny::req(input$activity_type)

          include <- input[[include_type]]

          slider_values[[at]][[.x]] <- shiny::req(input[[.i]])
          params[["non-demographic_adjustment"]][[at]][[.x]] <- if (include) slider_values[[at]][[.x]] / 100
        }) |>
          shiny::bindEvent(input[[.i]], input[[include_type]])

        shiny::observe({
          shinyjs::toggleState(.i, condition = input[[include_type]])
        }) |>
          shiny::bindEvent(input[[include_type]])
      }
    )
  })
}
