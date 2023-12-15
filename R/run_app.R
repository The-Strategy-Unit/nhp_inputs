#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart = NULL, # nolint
                    options = list(
                      port = as.numeric(Sys.getenv("GOLEM_PORT", 8080)),
                      host = Sys.getenv("GOLEM_HOST", "127.0.0.1")
                    ),
                    enableBookmarking = NULL, # nolint
                    uiPattern = "/", # nolint
                    ...) {
  if (getOption("golem.app.prod", FALSE)) {
    create_data_cache()
  }

  # required for async promise calls
  if (!is_local()) {
    future::plan(future::multicore, workers = 2)
  }

  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
