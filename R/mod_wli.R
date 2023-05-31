mod_wli_table <- function() {
  rtt_specialties() |>
    dplyr::mutate(
      ip_id = paste0("wli_ip_", .data$code),
      op_id = paste0("wli_op_", .data$code)
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

  numeric_input_gt <- function(x, inputid, ...) {
    as.character(
      shiny::numericInput(
        ns(inputid),
        label = NULL,
        value = 0,
        min = 0,
        ...
      )
    ) |>
      gt::html()
  }

  table <- mod_wli_table() |>
    dplyr::mutate(
      ip_input = purrr::map2(.data$code, .data$ip_id, numeric_input_gt),
      op_input = purrr::map2(.data$code, .data$op_id, numeric_input_gt)
    ) |>
    dplyr::select("specialty", tidyselect::ends_with("input")) |>
    gt::gt(rowname_col = "specialty") |>
    gt::cols_label(
      ip_input = "Inpatients",
      op_input = "Outpatients"
    )

  shiny::fluidRow(
    bs4Dash::box(
      collapsible = FALSE,
      headerBorder = FALSE,
      width = 4,
      md_file_to_html("app", "text", "wli.md")
    ),
    bs4Dash::box(
      collapsible = FALSE,
      headerBorder = FALSE,
      width = 8,
      table |>
        gt::as_raw_html()
    )
  )
}

#' wli Server Functions
#'
#' @noRd
mod_wli_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    init <- shiny::observe({
      # initialise the params
      params[["waiting_list_adjustment"]] <- list(
        ip = list(),
        op = list()
      )

      init$destroy()
    })

    table <- mod_wli_table() |>
      tidyr::pivot_longer(
        tidyselect::ends_with("id"),
        names_to = "activity_type",
        values_to = "id"
      ) |>
      dplyr::mutate(
        dplyr::across("activity_type", ~ stringr::str_sub(.x, 1, 2))
      )

    shiny::observe({
      shiny::req(session$userData$data_loaded())
      p <- shiny::req(session$userData$params$waiting_list_adjustment)

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
            params[["waiting_list_adjustment"]][[activity_type]][[code]] <- if (v > 0) v
          })
        }
      )
  })
}
