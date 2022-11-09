#' debug_params UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_debug_params_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("DEBUG: selected parameter values"),
    shiny::verbatimTextOutput(ns("params_json"))
  )
}

#' debug_params Server Functions
#'
#' @noRd
mod_debug_params_server <- function(id, expat_repat, activity_mitigators) {
  shiny::moduleServer(id, function(input, output, session) {
    output$params_json <- shiny::renderPrint({
      f <- purrr::compose(
        \(.x) .x[sort(names(.x))],
        purrr::flatten,
        purrr::map
      )

      am <- activity_mitigators |>
        purrr::map_depth(2, f, shiny::reactiveValuesToList) |>
        purrr::map_depth(3, \(.x) {
          if (is.null(.x)) {
            return(.x)
          }
          .x$interval <- .x$param_output(.x$rate, .x$interval)
          .x$param_output <- NULL
          .x$rate <- NULL
          .x
        })

      c(
        shiny::reactiveValuesToList(expat_repat),
        am
      ) |>
        jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
    })
  })
}
