# recursive future promise
mod_run_model_submit <- function(
  params_json,
  app_version,
  status,
  results_url
) {
  req <- httr2::request(Sys.getenv("NHP_API_URI")) |>
    httr2::req_url_path("api", "run_model") |>
    httr2::req_url_query(
      app_version = app_version,
      code = Sys.getenv("NHP_API_KEY")
    ) |>
    httr2::req_body_raw(params_json, "application/json") |>
    httr2::req_method("POST")

  httr2::req_perform_promise(req) |>
    promises::then(
      \(response) {
        results <- httr2::resp_body_json(response)

        status("Submitted Model Run")

        version <- results$app_version
        # generate the results url
        # nolint start: object_usage_linter
        f <- encrypt_filename(
          file.path(
            "prod",
            version,
            results$dataset,
            glue::glue("{results$scenario}-{results$create_datetime}.json.gz"),
            fsep = "/"
          )
        )
        # nolint end

        # update version for the url
        version <- stringr::str_replace(
          version,
          "^v(\\d)+\\.(\\d+).*",
          "v\\1-\\2"
        )
        results_url(
          glue::glue(
            Sys.getenv("NHP_OUTPUTS_URI"),
            "?{utils::URLencode(f, TRUE)}"
          )
        )

        mod_run_model_check_container_status(results[["id"]], status)
      }
    ) |>
    promises::catch(
      \(error) {
        print(error$message)
        status(error$message)
      }
    )
}

mod_run_model_check_container_status <- function(id, status) {
  promises::future_promise(
    {
      # wait 10 seconds before checking
      Sys.sleep(10)
      req <- httr2::request(Sys.getenv("NHP_API_URI")) |>
        httr2::req_url_path("api", "model_run_status", id) |>
        httr2::req_url_query(code = Sys.getenv("NHP_API_KEY"))

      httr2::req_perform(req)
    },
    packages = character()
  ) |>
    promises::then(
      \(response) {
        print(response)
        res <- httr2::resp_body_json(response)

        if (res$state == "Terminated") {
          if (res$detail_status == "Completed") {
            status("Success")
          } else {
            status("Error: running the model")
          }
          return(NULL)
        }
        status("Modelling running")

        # recursive call
        mod_run_model_check_container_status(id, status)
      }
    ) |>
    promises::catch(
      \(...) {
        # recursive call
        mod_run_model_check_container_status(id, status)
      }
    )
}
