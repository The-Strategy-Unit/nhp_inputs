mod_non_demographic_adjustment_input_server <- function(parent_id, id, type, params) {
  shiny::moduleServer(paste(sep = "-", parent_id, id), function(input, output, session) {
    default_values <- get_golem_config(c("non-demographic_adjustment", type, id))

    init <- shiny::observe({
      y <- params$end_year - as.numeric(stringr::str_sub(params$start_year, 1, 4))

      shiny::updateSliderInput(
        session,
        "values",
        value = (default_values^y) * 100
      )

      shiny::updateCheckboxInput(session, "include", value = TRUE)

      init$destroy()
    })

    # init <- shiny::observe({
    #   p <- shiny::isolate({
    #     params[["non-demographic_adjustment"]][[type]][[id]]
    #   })

    #   if (is.null(p)) {
    #     shiny::updateCheckboxInput(session, "include", value = FALSE)
    #   } else {
    #     shiny::updateCheckboxInput(session, "include", value = TRUE)
    #     shiny::updateSliderInput(session, "values", value = p * 100)
    #   }

    #   init$destroy()
    # })

    # shiny::observe({
    #   shinyjs::toggleState("values", input$include)
    # }) |>
    #   shiny::bindEvent(input$include)

    shiny::reactive({
      list(
        include = input$include,
        values = input$values / 100
      )
    })
  })
}
