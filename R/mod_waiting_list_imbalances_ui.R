#' wli UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_waiting_list_imbalances_ui <- function(id) {
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

  table <- mod_waiting_list_imbalances_table() |>
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
          md_file_to_html("app", "text", "waiting_list_imbalances.md")
        ),
        mod_reasons_ui(ns("reasons")),
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
