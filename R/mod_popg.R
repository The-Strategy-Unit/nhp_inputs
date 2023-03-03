#' popg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

projections <- readRDS(app_sys("app", "data", "pop_projection_names.Rds"))|>
  dplyr::mutate(startup_val =
                  dplyr::case_when(
                    model_name == 'principal_proj' ~ 100,
                    T ~ 0))



#
# projection_id <- projections$model_name
# projection_label <- projections$`Type of projection`
# startup_val <- c(100, rep(0, nrow(projections)-1))


mod_popg_ui <- function(id){
  ns <- NS(id)

  make_projection_sliders <- \(.x, .y, .z){
    shiny::sliderInput(ns(.x),
                       .y,
                       min = 0,
                       max = 100,
                       value = .z)
  }

  shiny::tagList(
    shinyjs::useShinyjs(),
    bs4Dash::box(
      title = "debug",
      shiny::verbatimTextOutput(ns("tmp"), placeholder = TRUE)
    ),
    purrr::pmap(projections,
              ~make_projection_sliders(..1, ..2,..3))
  )

}

#' popg Server Functions
#'
#' @noRd
mod_popg_server <- function(id, params){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    reduce_sliders_proportionally <- function(slider){
      shiny::updateSliderInput(inputId = slider,
                               value =
                                 input[[slider]] -
                                 (slider_params$others_sum - 100)*
                                 (input[[slider]]/slider_params$others_sum)
      )}

    shinyjs::disable('principal_proj')

    slider_params <- shiny::reactiveValues(others_sum = 0)


    shiny::observe({

      sliders_vals <- reactiveValuesToList(input)


      slider_params$others_sum <- do.call(sum, sliders_vals) - input$principal_proj

      output$tmp <- renderText(slider_params$others_sum)

      slider_params$principal_val <- 100 - slider_params$others_sum


      shiny::updateSliderInput(inputId = 'principal_proj',
                        value = slider_params$principal_val)


      if(slider_params$others_sum > 100){
        sliders <- projections$model_name

        purrr::map(sliders, reduce_sliders_proportionally)

        if(slider_params$others_sum > 100){
          slider <- names(which.max(sliders_vals))
          shiny::updateSliderInput(inputId = slider,
                                   value = input[[slider]] -
                                     (slider_params$others_sum - 100))
          }
        }
      })

    shiny::observe({
      params[["population_growth"]] <- reactiveValuesToList(input) |>
        purrr::map_depth(1, `/`, 100)
    })



  })
}

## To be copied in the UI
# mod_popg_ui("popg_1")

## To be copied in the server
# mod_popg_server("popg_1")
