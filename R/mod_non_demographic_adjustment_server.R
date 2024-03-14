#' Non Demographic Server Functions
#'
#' @noRd
mod_non_demographic_adjustment_server <- function(id, params) {

  mod_reasons_server(shiny::NS(id, "reasons"), params, "non-demographic_adjustment")

  shiny::moduleServer(id, function(input, output, session) {

    # reactives ----

    non_demographic_adjustment <- shiny::reactive({
      ndg_variants <- readr::read_rds(app_sys("app", "data", "ndg_variants.rds"))
      ndg_variants[[input$ndg_variant]]
    })

    # observers ----

    shiny::observe({
      params[["non-demographic_adjustment"]] <- non_demographic_adjustment()
    })

    # renders ----

    output$non_demographic_adjustment_table <- gt::render_gt({
      mod_non_demographic_adjustment_table(non_demographic_adjustment())
    })

  })

}
