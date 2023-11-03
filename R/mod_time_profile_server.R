#' time_profile Server Functions
#'
#' @noRd
mod_time_profile_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    update_time_profile <- shiny::reactiveVal()

    # if the baseline year, or the end year changes, update the slider
    shiny::observe({
      start <- as.numeric(stringr::str_sub(params$start_year, 1, 4))
      end <- params$end_year

      shiny::updateSliderInput(
        session,
        "step_year",
        min = start,
        max = end,
        value = start
      )
    })

    # show/hide the step year slider based on what is selected for the time profile
    shiny::observe({
      shinyjs::toggle(
        "step_year",
        condition = input$time_profile == "step_change"
      )
    })

    # if the selection changes
    shiny::observe({
      tp <- shiny::req(update_time_profile())
      if (stringr::str_sub(tp, 1, 4) == "step") {
        shiny::updateSliderInput(
          session,
          "step_year",
          value = as.numeric(stringr::str_sub(tp, 5))
        )
        tp <- "step_change"
      }

      shiny::updateSelectInput(
        session,
        "time_profile",
        selected = tp
      )
    }) |>
      shiny::bindEvent(update_time_profile())

    # update the time profile mappings when selections change
    selected_time_profile <- shiny::reactive({
      tp <- input$time_profile
      sy <- input$step_year

      ifelse(tp == "step_change", glue::glue("step{sy}"), tp)
    }) |>
      shiny::bindEvent(input$time_profile, input$step_year)

    return(
      list(
        selected_time_profile = selected_time_profile,
        update_time_profile = update_time_profile
      )
    )
  })
}
