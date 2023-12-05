#' inequalities Server Functions
#'
#' @noRd
mod_inequalities_server <- function(id, params) {
  selected_time_profile <- update_time_profile <- NULL
  c(selected_time_profile, update_time_profile) %<-% mod_time_profile_server(
    shiny::NS(id, "time_profile"),
    params
  )

  mod_reasons_server(shiny::NS(id, "reasons"), params, "inequalities")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      params[["inequalities"]] <- shiny::req(input$change)
    }) |>
      shiny::bindEvent(input$change)

    shiny::observe({
      params$time_profile_mappings[["inequalities"]] <- selected_time_profile()
    }) |>
      shiny::bindEvent(selected_time_profile())

    # when the module is initialised, load the values from the loaded params file
    init <- shiny::observe(
      {
        p <- shiny::isolate({
          params
        })

        update_time_profile(p$time_profile_mappings[["inequalities"]])

        shiny::updateSelectInput(
          session,
          "change",
          selected = p$inequalities %||% "no_change"
        )

        init$destroy()
      },
      priority = 10 # this observer needs to trigger before the dropdown change observer
    )
  })
}
