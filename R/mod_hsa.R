#' hsa UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hsa_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    htmltools::h1('Slider'),
    shiny::sliderInput(ns('slider'),
                       label = 'slider',
                       min = 0,
                       max = 200,
                       post = '%',
                       value = c(40, 60)
                       ),
    bs4Dash::box(
      title = "debug",
      shiny::verbatimTextOutput(ns('tmp'), placeholder = TRUE)
    )

  )
}

#' hsa Server Functions
#'
#' @noRd
mod_hsa_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    slider_val <- shiny::reactive({
      input$slider |>
        as.list() |>
        purrr::set_names(c("minVal", "maxVal"))
    })

    shiny::observe({
      print(req(slider_val()))
    })

    output$tmp <- shiny::renderPrint({
      paste0(
        "Our values are [",
        paste(input$slider, collapse = ", "),
        "]"
      )
    })

    return(slider_val)
  })
}

## To be copied in the UI
# mod_hsa_ui("hsa_1")

## To be copied in the server
# mod_hsa_server("hsa_1")
