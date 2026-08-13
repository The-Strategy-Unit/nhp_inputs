#' run_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_run_model_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    shiny::tagList(
      bslib::card(
        fill = FALSE,
        md_file_to_html("app", "text", "run_model.md")
      ),
      mod_reasons_ui(ns("reasons"))
    ),

    shiny::tagList(
      shinyjs::hidden(
        shiny::div(
          id = ns("model_run_args"),
          bslib::card(
            bslib::card_header(
              "Run Model",
              class = "bg-primary"
            ),
            fill = FALSE,
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              shinyjs::disabled(
                shiny::checkboxInput(
                  ns("results_viewable"),
                  "Make results viewable to all users",
                  value = TRUE
                )
              ),
              shinyjs::disabled(
                shiny::checkboxInput(
                  ns("full_model_results"),
                  "Save full model results",
                  value = FALSE
                )
              ),
              shiny::actionButton(
                ns("submit"),
                "Submit Model Run",
                class = "bg-success"
              ),
            )
          )
        )
      ),
      bslib::card(
        bslib::card_header(
          "Model Parameters",
          class = "bg-primary"
        ),
        fill = FALSE,
        bslib::layout_columns(
          col_widths = c(6, 6),

          shiny::downloadButton(
            ns("download_params"),
            "Download params",
            class = "bg-secondary"
          ),
          shiny::actionButton(
            ns("view_params"),
            "View params",
            class = "bg-secondary"
          )
        ),
        shiny::htmlOutput(ns("validation_errors")),
        shiny::uiOutput(ns("status"))
      )
    )
  )
}
