rtt_specialties <- function() {
  readRDS(app_sys("app", "data", "rtt_specialties.Rds")) |>
    as.list() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "specialty",
      values_to = "code"
    ) |>
    dplyr::select("code", "specialty")
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
  gt::gt_output(ns("rtt_table"))
}

#' wli Server Functions
#'
#' @noRd
mod_wli_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    numeric_input_gt <- function(x, inputid, ...) {
      as.character(
        shiny::numericInput(
          ns(paste0(inputid, x)),
          label = NULL,
          value = 0,
          min = 0,
          ...
        )
      ) |>
        gt::html()
    }

    table <- rtt_specialties() |>
      dplyr::mutate(
        Inpatients = purrr::map(.data$code, numeric_input_gt, "wli_ip_"),
        Outpatients = purrr::map(.data$code, numeric_input_gt, "wli_op_")
      )

    output$rtt_table <- gt::render_gt(table)

    purrr::walk(
      table$code,
      \(.x) {
        ip <- glue::glue("wli_ip_{.x}")
        shiny::observe({
          v <- shiny::req(input[[ip]])
          params[["waiting_list_adjustment"]][["ip"]][[.x]] <- if (v > 0) v
        })

        op <- glue::glue("wli_op_{.x}")
        shiny::observe({
          v <- shiny::req(input[[op]])
          params[["waiting_list_adjustment"]][["op"]][[.x]] <- if (v > 0) v
        })
      }
    )
  })
}
