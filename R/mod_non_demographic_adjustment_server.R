#' Non Demographic Server Functions
#'
#' @noRd
mod_non_demographic_adjustment_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    # reactives ----

    # load the non-demographic adjustment values
    non_demographic_adjustment <- shiny::reactive({
      return(
        list(
          "aae" = list(
            "ambulance" = c(1.0117592, 1.014443),
            "walk-in" = c(1.0117592, 1.014443)
          ),
          "ip" = list(
            "elective" = c(1.0050266, 1.007375),
            "maternity" = c(0.9732562, 0.980959),
            "non-elective" = c(1.0187719, 1.024636)
          ),
          "op" = list(
            "first" = c(1.0222222, 1.027585),
            "followup" = c(1.0222222, 1.027585),
            "procedure" = c(1.0222222, 1.027585)
          )
        )
      )
    })

    # observers ----

    # the non-demographic adjustment values are fixed, load them straight into the parameters
    shiny::observe({
      params[["non-demographic_adjustment"]] <- non_demographic_adjustment()
    })

    # renders ----

    # shows the selected values for the non-demographic adjustment
    output$non_demographic_adjustment_table <- gt::render_gt({
      mod_non_demographic_adjustment_table(non_demographic_adjustment())
    })
  })
}
