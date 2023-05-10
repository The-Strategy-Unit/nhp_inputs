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
    bs4Dash::box(
      title = "Ward Groups",
      shiny::selectInput(
        ns("ward_group"),
        "Ward Group",
        choices = "Other"
      ),
      shiny::fluidRow(
        col_6(
          shiny::numericInput(
            ns("baseline_hours"),
            "Baseline Hours",
            value = 365 * 24,
            min = 0
          )
        ),
        col_6(
          shiny::numericInput(
            ns("future_hours"),
            "Future Hours",
            value = 365 * 24,
            min = 0
          )
        )
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
      shiny::textOutput(ns("parameter_value")),
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
    col_6(
      shiny::uiOutput(ns("specialty_mapping"))
    )
  )
}

mod_bed_occupancy_specialty_table <- function(specialties, ns = identity) {
  shiny::fluidRow(
    purrr::pmap(
      specialties,
      \(code, specialty) {
        shiny::tagList(
          col_6(glue::glue("{specialty} ({code})")),
          col_6(
            shiny::selectInput(
              ns(glue::glue("specialty_{code}")),
              NULL,
              "Other",
              width = "100%"
            )
          )
        )
      }
    )
  )
}

#' bed_occupancy Server Functions
#'
#' @noRd
mod_bed_occupancy_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    default_param_values <- list(
      baseline_hours = 365 * 24,
      future_hours = 365 * 24,
      occupancy = c(85, 95)
    )

    ward_groups <- shiny::reactiveValues(
      groups = list(Other = default_param_values)
    )

    ward_group_names <- shiny::reactive({
      wgs <- names(ward_groups$groups)

      c(sort(setdiff(wgs, "Other")), "Other")
    })

    # disable the remove button if the selected group is "Other"
    shiny::observe({
      wg <- input$ward_group

      shinyjs::toggleState("remove_ward_group", wg != "Other")
    }) |>
      shiny::bindEvent(input$ward_group)

    # the add group button
    shiny::observe({
      ns <- session$ns

      modal <- shiny::modalDialog(
        title = "Add Ward Group",
        easyClose = FALSE,
        footer = NULL,
        shiny::textInput(ns("new_ward_group"), "Name"),
        shiny::fluidRow(
          col_6(
            bs4Dash::actionButton(
              ns("add_ward_group"),
              "Add Group",
              icon = shiny::icon("add"),
              width = "100%"
            )
          ),
          col_6(
            shiny::modalButton("Cancel")
          )
        )
      )

      shiny::showModal(modal)
    }) |>
      shiny::bindEvent(input$show_ward_group_modal)

    # the add button in the modal dialog
    shiny::observe({
      wg <- input$new_ward_group
      v <- default_param_values

      ward_groups$groups[[wg]] <- v
      shiny::updateNumericInput(session, "baseline_hours", value = v$baseline_hours)
      shiny::updateNumericInput(session, "future_hours", value = v$future_hours)
      shiny::updateSliderInput(session, "occupancy", value = v$occupancy)

      shiny::updateSelectInput(
        session,
        "ward_group",
        choices = ward_group_names(),
        selected = wg
      )

      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$add_ward_group)

    # the remove button
    shiny::observe({
      wg <- input$ward_group

      ward_groups$groups[[wg]] <- NULL
      params[["bed_occupancy"]][["day+night"]][[wg]] <- NULL

      shiny::updateSelectInput(
        session,
        "ward_group",
        choices = ward_group_names(),
        selected = "Other"
      )
    }) |>
      shiny::bindEvent(input$remove_ward_group)

    # ward_group dropdown changed
    shiny::observe({
      wg <- input$ward_group
      v <- ward_groups$groups[[wg]]

      shiny::updateNumericInput(session, "baseline_hours", value = v$baseline_hours)
      shiny::updateNumericInput(session, "future_hours", value = v$future_hours)
      shiny::updateSliderInput(session, "occupancy", value = v$occupancy)
    }) |>
      shiny::bindEvent(input$ward_group)

    #
    specialties <- readr::read_csv(
      app_sys("app/data/bed_occupancy_specialties.csv"),
      col_types = "ccc"
    ) |>
      dplyr::group_nest(.data$group) |>
      tibble::deframe()

    # render the specialty table
    output$specialty_mapping <- shiny::renderUI({
      purrr::imap(
        specialties,
        \(.x, .i) {
          bs4Dash::box(
            title = glue::glue("Specialty Mapping: {.i}"),
            width = 12,
            mod_bed_occupancy_specialty_table(.x, session$ns)
          )
        }
      )
    })

    # update the specialty dropdowns
    shiny::observe({
      wgs <- ward_group_names()

      purrr::walk(
        specialties |>
          purrr::map("code") |>
          purrr::flatten_chr(),
        \(.x) {
          .i <- glue::glue("specialty_{.x}")

          current_value <- input[[.i]]
          new_value <- ifelse(.i %in% wgs, current_value, "Other")

          shiny::updateSelectInput(
            session,
            .i,
            choices = ward_group_names(),
            selected = new_value
          )
        }
      )
    }) |>
      shiny::bindEvent(ward_group_names())

    # add observers for the specialty dropdowns
    purrr::walk(
      specialties |>
        purrr::map("code") |>
        purrr::flatten_chr(),
      \(.x) {
        shiny::observe({
          value <- input[[glue::glue("specialty_{.x}")]]

          params[["bed_occupancy"]][["specialty_mapping"]][["General and Acute"]][[.x]] <- value
        }) |>
          shiny::bindEvent(input[[glue::glue("specialty_{.x}")]])
      }
    )

    # update the params
    shiny::observe({
      wg <- input$ward_group
      occ_pcnt <- input$occupancy / 100
      hours <- input$future_hours / input$baseline_hours

      ward_groups$groups[[wg]] <- list(
        baseline_hours = input$baseline_hours,
        future_hours = input$future_hours,
        occupancy = input$occupancy
      )
      params[["bed_occupancy"]][["day+night"]][[wg]] <- occ_pcnt / hours
    }) |>
      shiny::bindEvent(input$baseline_hours, input$future_hours, input$occupancy)


    #
    output$parameter_value <- shiny::renderText({
      wg <- input$ward_group
      v <- params[["bed_occupancy"]][["day+night"]][[wg]]

      glue::glue(
        "Parameter Value: ",
        "{scales::percent(v[[1]], accuracy = 0.01)}",
        " to ",
        "{scales::percent(v[[2]], accuracy = 0.01)}"
      )
    })
  })
}
