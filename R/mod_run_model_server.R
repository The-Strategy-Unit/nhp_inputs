#' run_model Server Functions
#'
#' @noRd
mod_run_model_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "model_run")

  shiny::moduleServer(id, function(input, output, session) {
    # use a callback to handle the asynchronous promise when we submit the model run
    # see (https://stackoverflow.com/a/69451122)
    progress_callback <- shiny::reactiveVal()

    # the params as they are created in the app are not quite ready for use by
    # the model, this reactive handles this by "fixing" the params
    fixed_params <- shiny::reactive({
      shiny::req(params$scenario != "")

      p <- params |>
        shiny::reactiveValuesToList() |>
        mod_run_model_fix_params()

      # remove the inputs app data as this is not needed for the model run
      p[["__inputs_app__"]] <- NULL

      p
    })

    # output the status of the model run after submit is pressed
    output$status <- shiny::renderUI({
      s <- shiny::req(progress_callback())

      # if the progress callback is not a model_run_id, then display the message as is
      if (!inherits(s, "progress.model_run_id")) {
        return(s)
      }

      # handle the case where the model run has been submitted and we have a model_run_id
      progress_url <- glue::glue(
        "{envvars$NHP_MODEL_RUN_PROGRESS_URI)}?model_run_id={s[['dataset']]}/{s[['model_run_id']]}",
      )

      # add the model_run_id to the params
      params[["__inputs_app__"]][["model_run_id"]] <- s[["model_run_id"]]

      # redirect the user to the progress page...
      shinyjs::runjs(
        glue::glue("window.location.replace('{progress_url}');")
      )

      # ... but show a link in case the redirect fails
      shiny::tags$a(href = progress_url, "View Results")
    })

    # observe the submit button being pressed
    shiny::observe({
      shiny::req(input$submit)
      # immediately disable the submit button and the menu for the rest of the app
      shinyjs::disable("submit")
      shinyjs::hide(selector = "#sidebarItemExpanded")
      progress_callback(
        structure(
          "Please Wait...",
          class = "progress.running"
        )
      )

      # get the params
      p <- shiny::req(fixed_params())
      j <- shiny::req(params_json())

      # decide whether the results are viewable to all users: if this is false
      # then only nhp_devs/nhp_power_users can view the results
      viewable <- input$results_viewable
      full_model_results <- input$full_model_results

      # submit the model run
      mod_run_model_submit(
        j,
        p$app_version,
        viewable,
        full_model_results,
        progress_callback
      )

      # do not return the promise
      invisible(NULL)
    }) |>
      shiny::bindEvent(input$submit)

    # display the params as json
    params_json <- shiny::reactive({
      jsonlite::toJSON(fixed_params(), pretty = TRUE, auto_unbox = TRUE)
    })

    params_json_validation <- shiny::reactive({
      schema <- get_params_schema()

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

    shiny::observe({
      show_advanced_options <- any(
        c("nhp_devs", "nhp_power_users") %in% session$groups
      )
      enable_model_run_args <- show_advanced_options || is_local()

      shinyjs::toggle("model_run_args", enable_model_run_args)
      shinyjs::toggleState("results_viewable", enable_model_run_args)
      shinyjs::toggleState("full_model_results", enable_model_run_args)

      # by default, if we are in dev or it's a dev/power user, we should set
      # the results to not be viewable
      app_is_dev_version <- !stringr::str_starts(params$app_version, "v")
      shiny::updateCheckboxInput(
        session,
        "results_viewable",
        value = !(show_advanced_options || app_is_dev_version)
      )
    })

    # download the params when the download button is pressed
    # shiny downloadHandlers do not handle errors well, returning a .html file
    # instead of the intended content. we handle this by disabling the button
    # until the params are ready
    output$download_params <- shiny::downloadHandler(
      filename = \() paste0(fixed_params()$scenario, ".json"),
      content = \(file) {
        readr::write_lines(params_json(), file)
      }
    )
  })
}
