# recursive future promise
mod_run_model_submit <- function(params, status) {
  promises::future_promise(
    {
      httr::POST(
        Sys.getenv("NHP_API_URI"),
        path = c("api", "run_model"),
        query = list(
          app_version = Sys.getenv("NHP_CONTAINER_VERSION", "dev"),
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

        status("Submitted Model Run")

        mod_run_model_check_container_status(params$id, status)
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
