#' run_model Server Functions
#'
#' @noRd
mod_run_model_server <- function(id, params, schema) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "model_run")

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
        mod_run_model_fix_params(schema)
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

      # get the params
      p <- shiny::req(fixed_params())
      j <- shiny::req(params_json())

      # submit the model run
      mod_run_model_submit(j, p$app_version, status, results_url)

      # do not return the promise
      invisible(NULL)
    }) |>
      shiny::bindEvent(input$submit)

    # display the params as json
    params_json <- shiny::reactive({
      jsonlite::toJSON(fixed_params(), pretty = TRUE, auto_unbox = TRUE)
    })

    params_json_validation <- shiny::reactive({
      v <- schema$validate(params_json(), verbose = TRUE)

      list(
        # because v has attributes, force to be a simpler object
        is_valid = isTRUE(v),
        errors = attr(v, "errors")
      )
    })

    output$params_json <- shiny::renderText({
      v <- params_json_validation()

      shiny::validate(
        shiny::need(
          v$is_valid,
          "Error: invalid parameters, see validation errors below"
        )
      )

      params_json()
    })

    output$validation_errors <- gt::render_gt({
      v <- params_json_validation()
      ve_df <- v$errors

      shiny::req(ve_df)
      shiny::req(is.data.frame(ve_df) && nrow(ve_df) > 0)

      gt::gt(ve_df)
    })

    # observe the params - enable the submit / download button only when the
    # params are valid
    shiny::observe({
      v <- params_json_validation()$is_valid %||% FALSE

      shinyjs::toggleState("submit", condition = v && !input$submit)
      shinyjs::toggleState("download_params", condition = v)
    })

    # download the params when the download button is pressed
    # shiny downloadHandlers do not handle errors well, returning a .html file
    # instead of the intended content. we handle this by disabling the button
    # until the params are ready
    output$download_params <- shiny::downloadHandler(
      filename = \() paste0(fixed_params()$id, ".json"),
      content = \(file) {
        readr::write_lines(params_json(), file)
      }
    )
  })
}
