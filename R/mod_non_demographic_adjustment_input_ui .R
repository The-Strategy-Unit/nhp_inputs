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
      shinyjs::disabled({
        shiny::checkboxInput(
          ns(glue::glue("include")),
          "Include?"
        )
      })
    )
  )
}
