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
  shiny::fluidRow(
    col_4(
      bs4Dash::box(
        title = "Run Model",
        width = 12,
        shiny::fluidRow(
          col_6(
            shiny::actionButton(ns("submit"), "Submit Model Run"),
          ),
          col_6(
            shiny::downloadButton(ns("download_params"), "Download params")
          )
        ),
        shiny::uiOutput(ns("status"))
      ),
      bs4Dash::box(
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 12,
        md_file_to_html("app", "text", "run_model.md")
      )
    ),
    bs4Dash::box(
      title = "View Params",
      width = 8,
      collapsed = TRUE,
      shiny::verbatimTextOutput(ns("params_json"))
    )
  )
}

#' run_model Server Functions
#'
#' @noRd
mod_run_model_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    # we are using promises to run REST queries to submit a new model run
    # and to check on the progress of those model runs
    # because of limitations with shiny (https://stackoverflow.com/a/69451122)
    # we need to utilise some side effect to notify the user of the status of
    # the model runs
    status <- shiny::reactiveVal()
    # when a model run is run we insert the current time into create_datetime,
    # but this is kept within the job submission. this reactiveVal is used to
    # store the url of the results when they are complete
    results_url <- shiny::reactiveVal()

    # the params as they are created in the app are not quite ready for use by
    # the model, this reactive handles this by "fixing" the params
    fixed_params <- shiny::reactive({
      shiny::req(params$scenario != "")

      params |>
        shiny::reactiveValuesToList() |>
        mod_run_model_fix_params(session$user)
    })

    # output the status of the model run after submit is pressed
    output$status <- shiny::renderUI({
      s <- shiny::req(status())

      if (s == "Success") {
        shiny::tags$p(
          "Completed: ",
          shiny::tags$a(href = results_url(), "View Outputs")
        )
      } else {
        s
      }
    })

    # observe the submit button being pressed
    shiny::observe({
      shiny::req(input$submit)
      # immediately disable the submit button and the menu for the rest of the app
      shinyjs::disable("submit")
      shinyjs::hide(selector = "#sidebarItemExpanded")
      status("Please Wait...")

      # get the params and insert the current time for when the model run was
      # submitted
      p <- shiny::req(fixed_params())
      p$create_datetime <- format(Sys.time(), "%Y%m%d_%H%M%S")

      # generate the results url
      ds <- p$dataset
      sc <- utils::URLencode(p$scenario)
      cd <- p$create_datetime
      uri <- Sys.getenv("NHP_OUTPUTS_URI")
      results_url(glue::glue("{uri}#/{ds}/{sc}/{cd}"))

      # submit the model run
      mod_run_model_submit(p, status)

      # do not return the promise
      invisible(NULL)
    }) |>
      shiny::bindEvent(input$submit)

    # display the params as json
    output$params_json <- shiny::renderText({
      jsonlite::toJSON(fixed_params(), pretty = TRUE, auto_unbox = TRUE)
    })

    shiny::observe({
      p <- !is.null(tryCatch(fixed_params(), error = \(...) NULL))
    })

    # observe the params - enable the submit / download button only when the
    # params are valid
    shiny::observe({
      p <- !is.null(tryCatch(fixed_params(), error = \(...) NULL))

      shinyjs::toggleState("submit", condition = p && !input$submit)
      shinyjs::toggleState("download_params", condition = p)
    })

    # download the params when the download button is pressed
    # shiny downloadHandlers do not handle errors well, returning a .html file
    # instead of the intended content. we handle this by disabling the button
    # until the params are ready
    output$download_params <- shiny::downloadHandler(
      filename = \() paste0(fixed_params()$id, ".json"),
      content = \(file) jsonlite::write_json(fixed_params(), file, pretty = TRUE, auto_unbox = TRUE)
    )
  })
}
