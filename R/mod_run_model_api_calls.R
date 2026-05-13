# recursive future promise
mod_run_model_submit <- function(
  params_json,
  app_version,
  viewable,
  full_model_results,
  status,
  results_url
) {
  metadata <- params_json |>
    jsonlite::fromJSON() |>
    _[c("user", "dataset", "scenario")] |>
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

  cat(
    "model run submitted:\n",
    metadata,
    "\n",
    sep = ""
  )

  req <- httr2::request(Sys.getenv("NHP_API_URI")) |>
    httr2::req_url_path("api", "run_model") |>
    httr2::req_url_query(
      app_version = app_version,
      code = Sys.getenv("NHP_API_KEY"),
      save_full_model_results = tolower(as.character(full_model_results)),
      results_viewable = tolower(as.character(viewable))
    ) |>
    httr2::req_body_raw(params_json, "application/json") |>
    httr2::req_method("POST")

  httr2::req_perform_promise(req) |>
    promises::then(
      \(response) {
        results <- httr2::resp_body_json(response)

        cat(
          "success:\n",
          jsonlite::toJSON(results, pretty = TRUE, auto_unbox = TRUE),
          "\n"
        )

        status("Submitted Model Run")

        # update version for the url
        version <- stringr::str_replace(
          results$app_version,
          "^v(\\d)+\\.(\\d+).*",
          "v\\1-\\2"
        )
        if (!stringr::str_detect(version, "^v\\d+-\\d+$")) {
          version <- "dev"
        }
        results_url(
          glue::glue(
            Sys.getenv("NHP_OUTPUTS_URI"),
            "?{utils::URLencode(results$outputs_app_uri, TRUE)}"
          )
        )

        mod_run_model_check_container_status(
          results[["dataset"]],
          results[["model_run_id"]],
          status
        )
      }
    ) |>
    promises::catch(
      \(error) {
        cat("Error submitting model run: ", error$message, "\n", sep = "")
        status(error$message)
      }
    )
}

mod_run_model_check_container_status <- function(
  dataset,
  model_run_id,
  status,
  error_counter = 10
) {
  id <- glue::glue("{dataset} | {model_run_id}")
  if (error_counter == 0) {
    cat(
      "error checking status for model run id: ",
      id,
      ". too many attempts\n",
      sep = ""
    )
    status("Error: running the model")
    return(NULL)
  }
  cat("checking status for ", id, "\n", sep = "")

  promises::future_promise(
    {
      # wait 10 seconds before checking
      Sys.sleep(10)
      req <- httr2::request(Sys.getenv("NHP_API_URI")) |>
        httr2::req_url_path("api", "model_run_status", dataset, model_run_id) |>
        httr2::req_url_query(code = Sys.getenv("NHP_API_KEY"))

      httr2::req_perform(req)
    },
    packages = character()
  ) |>
    promises::then(
      \(response) {
        res <- httr2::resp_body_json(response)

        if (res$state == "Terminated") {
          if (res$detail_status == "Completed") {
            cat("model run success: ", id, "\n", sep = "")
            status("Success")
          } else {
            cat("model run error: ", id, "\n", res$error, "\n", sep = "")
            status(glue::glue("Error running the model ({id}): {res$error}"))
          }
          return(NULL)
        } else if (res$state == "Creating") {
          # no need to change status
        } else {
          progress <- res$complete
          model_runs <- res$model_runs

          if (is.null(progress)) {
            cat(
              "model run id: ",
              id,
              ", stage: saving results\n",
              sep = ""
            )
            status("Model running [saving results]")
          } else {
            if (progress$aae > 0 || progress$outpatients >= model_runs) {
              stage <- "A&E"
              complete <- progress$aae
            } else if (
              progress$outpatients > 0 || progress$inpatients >= model_runs
            ) {
              stage <- "Outpatients"
              complete <- progress$outpatients
            } else {
              stage <- "Inpatients"
              complete <- progress$inpatients
            }
            pcnt <- scales::percent(complete / model_runs, 0.1)

            cat(
              "model run id: ",
              id,
              ", stage: ",
              stage,
              " progress: ",
              complete,
              "/",
              model_runs,
              " (",
              pcnt,
              ")\n",
              sep = ""
            )

            status(glue::glue(
              "Model Running [{stage}: {complete}/{model_runs} ({pcnt})]"
            ))
          }
        }

        # recursive call
        mod_run_model_check_container_status(dataset, model_run_id, status, 10)
      }
    ) |>
    promises::catch(
      \(error) {
        cat("error:", error$message, "\n")
        # recursive call
        mod_run_model_check_container_status(
          dataset,
          model_run_id,
          status,
          error_counter - 1
        )
      }
    )
}
