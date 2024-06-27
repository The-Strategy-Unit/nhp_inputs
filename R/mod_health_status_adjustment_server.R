#' health_status_adjustment Server Functions
#'
#' @noRd
mod_health_status_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      g <- session$groups

      cat("groups:", g, "\n")
      if ((is.null(g) || any(c("nhp_devs", "nhp_power_users") %in% g))) {
        cat("enabling\n")
        shinyjs::show("enable_hsa")
        shinyjs::enable("enable_hsa")
      }
    })

    shiny::observe({
      params$health_status_adjustment <- input$enable_hsa
    }) |>
      shiny::bindEvent(input$enable_hsa)
  })
}
