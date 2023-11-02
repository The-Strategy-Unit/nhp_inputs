mod_bed_occupancy_specialties <- function() {
  readr::read_csv(
    app_sys("app/data/bed_occupancy_specialties.csv"),
    col_types = "ccc"
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

#' bed_occupancy Server Functions
#'
#' @noRd
mod_bed_occupancy_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "bed_occupancy")

  shiny::moduleServer(id, function(input, output, session) {
    default_param_values <- c(85, 95)

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
      # force the value to be inserted into the params, don't rely on observer
      params[["bed_occupancy"]][["day+night"]][[wg]] <- v
      shiny::updateSliderInput(session, "occupancy", value = v)

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
      shiny::updateSliderInput(
        session,
        "occupancy",
        value = ward_groups$groups[[input$ward_group]]
      )
    }) |>
      shiny::bindEvent(input$ward_group)

    #
    specialties <- mod_bed_occupancy_specialties()

    # update the specialty dropdowns
    shiny::observe({
      wgs <- ward_group_names()

      specialties |>
        purrr::pwalk(\(group, code, ...) {
          current_value <- params[["bed_occupancy"]][["specialty_mapping"]][[group]][[code]]
          value <- ifelse(current_value %in% wgs, current_value, "Other")

          shiny::updateSelectInput(
            session,
            glue::glue("specialty_{code}"),
            choices = wgs,
            selected = value
          )
        })
    }) |>
      shiny::bindEvent(ward_group_names(), session$userData$data_loaded())

    # add observers for the specialty dropdowns
    purrr::pwalk(
      specialties,
      \(group, code, ...) {
        shiny::observe({
          value <- input[[glue::glue("specialty_{code}")]]

          params[["bed_occupancy"]][["specialty_mapping"]][[group]][[code]] <- value
        }) |>
          shiny::bindEvent(input[[glue::glue("specialty_{code}")]])
      }
    )

    # param file uploaded
    shiny::observe({
      shiny::req(session$userData$data_loaded())
      p <- shiny::req(session$userData$params$bed_occupancy)
      params$bed_occupancy <- p

      ward_groups$groups <- p[["day+night"]] |>
        purrr::map(`*`, 100)

      shiny::updateSelectInput(
        session,
        "ward_group",
        choices = ward_group_names(),
        selected = "Other"
      )
      shiny::updateSliderInput(session, "occupancy", value = ward_groups$groups[["Other"]])
    }) |>
      shiny::bindEvent(session$userData$data_loaded())

    # update the params
    shiny::observe({
      wg <- input$ward_group
      occ <- input$occupancy

      ward_groups$groups[[wg]] <- occ
      params[["bed_occupancy"]][["day+night"]][[wg]] <- occ / 100
    }) |>
      shiny::bindEvent(input$ward_group, input$occupancy)
  })
}
