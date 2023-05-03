
theatre_specialties <- function(){
  readRDS(app_sys("app", "data", "theatre_specialties.Rds"))|>
    dplyr::mutate(baseline_spells = as.character(c(1.6, 1.7, 2.2, 1.8, 5.4, 2.8, 2, 2.4, 2.6)),
                  baseline_cases = as.character(c(1.4, 1, 1.1, 2.9, 1, 1.3, 1, 1.1, 1.2)))

}


#' theatres UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_theatres_ui <- function(id){
  ns <- NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    bs4Dash::box(
      title = "debug",
      shiny::tableOutput(ns('debug'))),
    gt::gt_output(ns('theatres_table'))
  )




}

#' theatres Server Functions
#'
#' @noRd
mod_theatres_server <- function(id, params){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    slider_input_gt <- function(x, inputid, baseline,...){
      as.character(
        shiny::sliderInput(
          ns(paste0(inputid, x)),
          label = paste('Baseline:', baseline),
          value = c(1,2),
          min = 0,
          max = 10,
          step = 0.1,
          ...
        )
      ) |>
        gt::html()
    }

    table2 <- theatre_specialties() |>
       dplyr::mutate(
        `Theatre spells per case (elective)` =
          purrr::map2(Code,
                      baseline_spells,
                      .f = ~req(slider_input_gt(.x, 'theatres_spellPerCase_', .y))),
        `Theatre cases per 4hr session (elective)` =
          purrr::map2(Code,
                      baseline_cases,
                      .f = ~req(slider_input_gt(.x, 'theatres_casePer4hr_', .y))))|>
      dplyr::select(-c(baseline_spells, baseline_cases))

    output$theatres_table <- gt::render_gt(table2)




    purrr::walk(table2$Code,
                \(.x) {

                  spells_id <- glue::glue('theatres_spellPerCase_{.x}')
                  cases_id <- glue::glue('theatres_casePer4hr_{.x}')

                  spells_at <- glue::glue('spellPerCase_{.x}')
                  cases_at <- glue::glue('casePer4hr_{.x}')

                  util_at <- glue::glue('util_{.x}')

                  shiny::observe({
                    params[["theatres"]][[spells_at]][[.x]] <- input[[spells_id]]
                  })

                  shiny::observe({
                    params[["theatres"]][[cases_at]][[.x]] <- input[[cases_id]]
                  })

                  shiny::observe({
                    params[["theatres"]][[util_at]][[.x]] <- 1/(input[[spells_id]][[1]]* input[[cases_id]][[1]])
                  })


                }
    )





  })
}

