#' bed_occupancy Server Functions
#'
#' @noRd
mod_bed_occupancy_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "bed_occupancy")

  shiny::moduleServer(id, function(input, output, session) {
    # static values ----
    default_param_values <- c(85, 95)

    specialties <- mod_bed_occupancy_specialties()

    # reactives ----

    # use a reactiveValues to store the list of ward groups that the user has created
    ward_groups <- shiny::reactiveValues(
      groups = list(Other = default_param_values)
    )

    # extract the names of the ward groups, make sure to put Other at the end of the list
    ward_group_names <- shiny::reactive({
      wgs <- names(ward_groups$groups)

      c(sort(setdiff(wgs, "Other")), "Other")
    })

    # observers ----

    # disable the remove button if the selected group is "Other"
    shiny::observe({
      wg <- input$ward_group

      shinyjs::toggleState("remove_ward_group", wg != "Other")
    }) |>
      shiny::bindEvent(input$ward_group)

    # the add group button (shows the add ward group modal)
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

    # add ward group modal: add button
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

    # add ward group modal: the remove button
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

    # when the ward_group dropdown changes, update the occupancy rate slider to use the value for that ward group
    shiny::observe({
      shiny::updateSliderInput(
        session,
        "occupancy",
        value = ward_groups$groups[[input$ward_group]]
      )
    }) |>
      shiny::bindEvent(input$ward_group)

    # when the ward groups are updated, update all of the specialty drop downs to include/remove the changed ward group
    shiny::observe({
      wgs <- ward_group_names()

      specialties |>
        purrr::pwalk(\(group, code, ...) {
          current_value <- params[["bed_occupancy"]][["specialty_mapping"]][[group]][[code]]
          # if a specialty was mapped to a ward group that no longer exists, set that specialty to "Other"
          value <- ifelse(current_value %in% wgs, current_value, "Other")

          shiny::updateSelectInput(
            session,
            glue::glue("specialty_{code}"),
            choices = wgs,
            selected = value
          )
        })
    }) |>
      shiny::bindEvent(ward_group_names())

    # when the specialty drop downs are changed, update the params
    purrr::pwalk(
      specialties,
      \(group, code, ...) {
        shiny::observe(
          {
            value <- input[[glue::glue("specialty_{code}")]]

            params[["bed_occupancy"]][["specialty_mapping"]][[group]][[code]] <- value
          },
          priority = -1
        ) |>
          shiny::bindEvent(input[[glue::glue("specialty_{code}")]])
      }
    )

    # when the slider changes for a ward group, update the params
    shiny::observe({
      wg <- input$ward_group
      occ <- input$occupancy

      ward_groups$groups[[wg]] <- occ
      params[["bed_occupancy"]][["day+night"]][[wg]] <- occ / 100
    }) |>
      shiny::bindEvent(input$ward_group, input$occupancy)

    # when the module is initialised, load the values from the loaded params file
    init <- shiny::observe(
      {
        p <- shiny::isolate({
          params$bed_occupancy
        })

        ward_groups$groups <- p[["day+night"]] |>
          purrr::map(`*`, 100)

        shiny::updateSelectInput(
          session,
          "ward_group",
          choices = ward_group_names(),
          selected = "Other"
        )
        shiny::updateSliderInput(session, "occupancy", value = ward_groups$groups[["Other"]])

        init$destroy()
      },
      priority = 20
    )
  })
}
