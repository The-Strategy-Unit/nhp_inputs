# recursive future promise
mod_run_model_submit <- function(
  params_json,
  app_version,
  viewable,
  full_model_results,
  progress_callback
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

  req <- httr2::request(envvars$NHP_API_URI) |>
    httr2::req_url_path("api", "run_model") |>
    httr2::req_url_query(
      app_version = app_version,
      code = envvars$NHP_API_KEY,
      save_full_model_results = tolower(as.character(full_model_results)),
      results_viewable = tolower(as.character(viewable))
    ) |>
    httr2::req_body_raw(params_json, "application/json") |>
    httr2::req_method("POST")

  httr2::req_perform_promise(req) |>
    promises::then(
      \(response) {
        results <- httr2::resp_body_json(response)

        progress_callback(
          structure(
            list(
              "dataset" = results[["dataset"]],
              "model_run_id" = results[["model_run_id"]]
            ),
            class = "progress.model_run_id"
          )
        )
      }
    ) |>
    promises::catch(
      \(error) {
        cat("Error submitting model run: ", error$message, "\n", sep = "")
        progress_callback(
          structure(
            error$message,
            class = "progress.error"
          )
        )
      }
    )
}
