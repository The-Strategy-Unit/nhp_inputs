#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(on_start = NULL,
                    options = list(port = 8080),
                    enable_bookmarking = NULL,
                    ui_pattern = "/",
                    ...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = on_start,
      options = options,
      enableBookmarking = enable_bookmarking,
      uiPattern = ui_pattern
    ),
    golem_opts = list(...)
  )
}
