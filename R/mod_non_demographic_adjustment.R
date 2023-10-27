nda_groups <- list(
  "ip" = list(
    name = "Inpatients",
    values = c(
      "elective" = "Elective",
      "non-elective" = "Non-Elective",
      "maternity" = "Maternity"
    )
  ),
  "op" = list(
    name = "Outpatients",
    values = c(
      "first" = "First",
      "followup" = "Follow-up",
      "procedure" = "Procedure"
    )
  ),
  "aae" = list(
    name = "Accident and Emergency",
    values = c(
      "ambulance" = "Ambulance",
      "walk-in" = "Walk-in"
    )
  )
)


mod_non_demographic_adjustment_input_ui <- function(parent_id, id, label) {
  ns <- shiny::NS(paste(sep = "-", parent_id, id))

  shiny::fluidRow(
    col_9(
      shiny::tags$div(
        class = "label-left",
        shinyjs::disabled(
          shiny::sliderInput(
            inputId = ns("values"),
            label = label,
            min = 0,
            max = 200,
            post = "%",
            value = c(100, 120),
            step = 0.01
          )
        )
      )
    ),
    col_3(
      shiny::checkboxInput(
        ns(glue::glue("include")),
        "Include?"
      )
    )
  )
}

mod_non_demographic_adjustment_input_server <- function(parent_id, id, type, params) {
  shiny::moduleServer(paste(sep = "-", parent_id, id), function(input, output, session) {
    default_values <- get_golem_config(c("non-demographic_adjustment", type, id))

    shiny::observe({
      y <- params$end_year - as.numeric(stringr::str_sub(params$start_year, 1, 4))

      shiny::updateSliderInput(
        session,
        "values",
        value = (default_values^y) * 100
      )

      shiny::updateCheckboxInput(session, "include", value = TRUE)
    })

    shiny::observe({
      shiny::req(session$userData$data_loaded())
      p <- shiny::req(session$userData$params[["non-demographic_adjustment"]][[type]][[id]])

      if (is.null(p)) {
        shiny::updateCheckboxInput(session, "include", value = FALSE)
      } else {
        shiny::updateCheckboxInput(session, "include", value = TRUE)
        shiny::updateSliderInput(session, "values", value = p * 100)
      }
    }) |>
      shiny::bindEvent(session$userData$data_loaded())

    shiny::observe({
      shinyjs::toggleState("values", input$include)
    }) |>
      shiny::bindEvent(input$include)

    shiny::reactive({
      list(
        include = input$include,
        values = input$values / 100
      )
    })
  })
}


#' Non Demographic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_non_demographic_adjustment_ui <- function(id) {
  ns <- shiny::NS(id)

  boxes <- purrr::map(
    nda_groups,
    \(.x) bs4Dash::box(
      collapsible = FALSE,
      headerBorder = FALSE,
      title = .x$name,
      width = 8,
      purrr::imap(
        .x$values,
        \(.x, .i) mod_non_demographic_adjustment_input_ui(id, .i, .x)
      )
    )
  )

  shiny::tagList(
    shiny::tags$h1("Non-demographic Adjustment"),
    shiny::fluidRow(
      col_4(
        mod_time_profile_ui(ns("time_profile")),
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "non_demographic_adjustment.md")
        )
      ),
      col_8(boxes)
    )
  )
}

#' Non Demographic Server Functions
#'
#' @noRd
mod_non_demographic_adjustment_server <- function(id, params) { # nolint: object_usage_linter.
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  nda_values <- purrr::imap(
    nda_groups,
    \(.x, .i) {
      purrr::map(
        purrr::set_names(names(.x$values)),
        \(.y) mod_non_demographic_adjustment_input_server(id, .y, .i, params)
      )
    }
  )

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      params[["non-demographic_adjustment"]] <- nda_values |>
        purrr::map(\(.x) {
          .x |>
            purrr::map(rlang::exec) |>
            purrr::keep("include") |>
            purrr::map("values")
        })
    })

    # update the time profile
    shiny::observe({
      params$time_profile_mappings[["non-demographic_adjustment"]] <- selected_time_profile()
    }) |>
      shiny::bindEvent(selected_time_profile())
  })
}
