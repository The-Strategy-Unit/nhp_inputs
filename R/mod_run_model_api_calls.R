# recursive future promise
mod_run_model_submit <- function(params, status, results_url) {
  promises::future_promise(
    {
      httr::POST(
        Sys.getenv("NHP_API_URI"),
        path = c("api", "run_model"),
        query = list(
          app_version = params$app_version,
          code = Sys.getenv("NHP_API_KEY")
        ),
        body = params,
        encode = "json"
      )
    },
    packages = character()
  ) |>
    promises::then(
      \(request) {
        response <- httr::status_code(request)
        if (response != 200) {
          status(paste("Error:", response))
          return(NULL)
        }

        results <- httr::content(request)

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
    )
}

mod_run_model_check_container_status <- function(id, status) {
  promises::future_promise(
    {
      # wait 10 seconds before checking
      Sys.sleep(10)
      httr::GET(
        Sys.getenv("NHP_API_URI"),
        path = c("api", "model_run_status", id),
        query = list(
          code = Sys.getenv("NHP_API_KEY")
        )
      )
    },
    packages = character()
  ) |>
    promises::then(
      \(request) {
        if (httr::status_code(request) == 200) {
          res <- httr::content(request)

          if (res$state == "Terminated") {
            if (res$detail_status == "Completed") {
              status("Success")
            } else {
              status("Error: running the model")
            }
            return(NULL)
          }
          status("Modelling running")
        }
        # recursive call
        mod_run_model_check_container_status(id, status)
      }
    )
}
