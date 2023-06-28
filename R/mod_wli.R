mod_wli_table <- function() {
  rtt_specialties() |>
    dplyr::mutate(
      ip_id = paste0("wli_ip_", sanitize_input_name(.data$code)),
      op_id = paste0("wli_op_", sanitize_input_name(.data$code))
    )
}
#' wli UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_wli_ui <- function(id) {
  ns <- shiny::NS(id)

  numeric_input_gt <- function(id, ...) {
    shiny::numericInput(ns(id), label = NULL, value = 0, ...) |>
      as.character() |>
      gt::html()
  }

  text_output_gt <- function(id, ...) {
    shiny::textInput(ns(paste0(id, "_output")), NULL, "0") |>
      shinyjs::disabled() |>
      as.character() |>
      gt::html()
  }

  table <- mod_wli_table() |>
    dplyr::mutate(
      ip_input = purrr::map(.data[["ip_id"]], numeric_input_gt),
      ip_output = purrr::map(.data[["ip_id"]], text_output_gt),
      op_input = purrr::map(.data[["op_id"]], numeric_input_gt),
      op_output = purrr::map(.data[["op_id"]], text_output_gt)
    ) |>
    dplyr::select(-"code", -"ip_id", -"op_id") |>
    gt::gt(rowname_col = "specialty") |>
    gt::tab_spanner(
      label = "Inpatients",
      columns = tidyselect::starts_with("ip")
    ) |>
    gt::tab_spanner(
      label = "Outpatients",
      columns = tidyselect::starts_with("op")
    ) |>
    gt::cols_label(
      ip_input = "Value",
      ip_output = "Change %",
      op_input = "Value",
      op_output = "Change %"
    ) |>
    gt::tab_options(table.width = gt::pct(100))

  shiny::tagList(
    shiny::tags$h1("Waiting List Imbalances"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "wli.md")
        ),
        mod_time_profile_ui(ns("time_profile"))
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 8,
        table |>
          gt::as_raw_html()
      )
    )
  )
}

#' wli Server Functions
#'
#' @noRd
mod_wli_server <- function(id, params) { # nolint: object_usage_linter.
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  shiny::moduleServer(id, function(input, output, session) {
    table <- mod_wli_table() |>
      tidyr::pivot_longer(
        tidyselect::ends_with("id"),
        names_to = "activity_type",
        values_to = "id"
      ) |>
      dplyr::mutate(
        dplyr::across("activity_type", ~ stringr::str_sub(.x, 1, 2))
      )

    init <- shiny::observe({
      # initialise the params
      params[["waiting_list_adjustment"]] <- list(
        ip = list(),
        op = list()
      )

      init$destroy()
    })

    # update the time profile
    shiny::observe({
      params$time_profile_mappings[["waiting_list_adjustment"]] <- selected_time_profile()
    }) |>
      shiny::bindEvent(selected_time_profile())

    baseline_data <- shiny::reactive({
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))

      df <- load_rds_from_adls(glue::glue("{dataset}/waiting_list_adjustment.rds"))[[year]]

      tretspef <- df[["tretspef"]]
      list("ip", "op") |>
        purrr::set_names() |>
        purrr::map(\(.x) purrr::set_names(as.list(df[[.x]]), tretspef))
    })

    shiny::observe({
      bd <- shiny::req(baseline_data())

      table |>
        purrr::pwalk(\(activity_type, id, code, ...) {
          v <- bd[[activity_type]][[code]] %||% 0
          if (v == 0) {
            shinyjs::disable(id)
          }
        })
    }) |>
      shiny::bindEvent(baseline_data())

    shiny::observe({
      shiny::req(session$userData$data_loaded())
      p <- shiny::req(session$userData$params$waiting_list_adjustment)

      # update the selected time profile
      update_time_profile(session$userData$params$time_profile_mappings[["waiting_list_adjustment"]])

      table |>
        purrr::pwalk(\(activity_type, id, code, ...) {
          v <- p[[activity_type]][[code]] %||% 0

          shiny::updateNumericInput(session, id, value = v)
        })
    }) |>
      shiny::bindEvent(session$userData$data_loaded())

    table |>
      purrr::pwalk(
        \(activity_type, id, code, ...) {
          shiny::observe({
            v <- shiny::req(input[[id]])
            bd <- shiny::req(baseline_data())[[activity_type]][[code]] %||% 0

            change <- v / bd

            change_string <- scales::percent(change, 0.01)

            shiny::updateTextInput(session, paste0(id, "_output"), value = change_string)

            params[["waiting_list_adjustment"]][[activity_type]][[code]] <- if (v > 0) v
          })
        }
      )
  })
}
