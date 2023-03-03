
rtt_specialties <- function(){
  readRDS(app_sys("app", "data", "rtt_specialties.Rds"))|>
    as.list()|>
    tibble::as_tibble()|>
    tidyr::pivot_longer(cols = everything(),
                        names_to = 'Specialty',
                        values_to = 'Code')|>
    dplyr::select(Code, Specialty)
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
mod_wli_ui <- function(id){
  ns <- NS(id)
  gt::gt_output(ns('rttTable'))
    #shiny::tableOutput(ns('rttTable')),
    # purrr::map(
    #   rtt_specialties(),
    #   \(.x) {
    #     shiny::fluidRow(
    #       col_6(
    #         HTML(.x)
    #       ),
    #       col_3(shiny::numericInput(ns(glue::glue('wli_ip_{.x}')),
    #                                 label = NULL,
    #                                 value = 0,
    #                                 min = 0)),
    #       col_3(shiny::numericInput(ns(glue::glue('wli_op_{.x}')),
    #                                 label = NULL,
    #                                 value = 0,
    #                                 min = 0))
    #       )
    #     }
    #   )

  }

#' wli Server Functions
#'
#' @noRd
mod_wli_server <- function(id, params){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    numericInput_gt <- function(x, inputid, ...){
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
        Inpatients = purrr::map(Code, .f = ~req(numericInput_gt(.x, 'wli_ip_'))),
        Outpatients = purrr::map(Code, .f = ~req(numericInput_gt(.x, 'wli_op_'))))



    output$rttTable <- gt::render_gt(table)



      purrr::walk(table$Code,
                  \(.x) {

                    ip_at <- glue::glue('inpatients_{.x}')
                    op_at <- glue::glue('outpatients_{.x}')

                    ip_id <- glue::glue('wli_ip_{.x}')
                    op_id <- glue::glue('wli_op_{.x}')

                    shiny::observe({
                      params[["waiting_list_imbalance"]][[ip_at]][[.x]] <- input[[ip_id]]
                      })

                    shiny::observe({
                      params[["waiting_list_imbalance"]][[op_at]][[.x]] <- input[[op_id]]
                    })
                    }
                  )




  })
}

## To be copied in the UI
# mod_wli_ui("wli_1")

## To be copied in the server
# mod_wli_server("wli_1")
