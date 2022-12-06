#' nda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nda_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    tags$style(HTML(
      "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
    )),
    shiny::selectInput(ns('dropdown'),
                       label = 'Activity Type',
                       choices = c('Non-Elective',
                                   'Elective',
                                   'Maternity') |>
                         purrr::set_names() |>
                         purrr::map_chr(stringr::str_to_lower)),

    purrr::map(
      c(' 0- 4',
        ' 5-14',
        '15-34',
        '35-49',
        '50-64',
        '65-84',
        '85+'),
      function(x) {
        div(class = 'label-left',
            shiny::sliderInput(
              inputId = ns(paste('slider', x)),
              label = (x),
              min = 0,
              max = 200,
              post = '%',
              value = c(50,150)
              )
        )
        }
      ),

    bs4Dash::box(
      title = "debug",
      shiny::verbatimTextOutput(ns('tmp'), placeholder = TRUE)
    )

  )
}

#' nda Server Functions
#'
#' @noRd
mod_nda_server <- function(id, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$tmp <- shiny::renderPrint({
      paste0(
        "Our values are [",
        paste(input$dropdown, collapse = ", "),
        "]"
      )
    })
  })
}

## To be copied in the UI
# mod_nda_ui("nda_1")

## To be copied in the server
# mod_nda_server("nda_1")
