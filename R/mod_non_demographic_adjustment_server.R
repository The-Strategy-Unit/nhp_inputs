#' Non Demographic Server Functions
#'
#' @noRd
mod_non_demographic_adjustment_server <- function(id, params) {

  mod_reasons_server(shiny::NS(id, "reasons"), params, "non-demographic_adjustment")

  shiny::moduleServer(id, function(input, output, session) {

    ndg_variants <- readr::read_rds(app_sys("app", "data", "ndg_variants.rds"))

    # reactives ----

    non_demographic_adjustment <- shiny::reactive({
      ndg_variants[[input$ndg_variant]]
    })

    # observers ----

    shiny::observe({
      can_select_variant <-
        is_local() || any(c("nhp_devs", "nhp_run_model") %in% session$groups)
      shinyjs::toggle("ndg_variant_dropdown", condition = can_select_variant)
    })

    shiny::observe({
      params[["non-demographic_adjustment"]] <- non_demographic_adjustment()
    }) |>
      shiny::bindEvent(non_demographic_adjustment())

    init <- shiny::observe({

      p_ndg <- shiny::isolate({
        params[["non-demographic_adjustment"]]
      })

      detected_ndg_variant <- detect_non_demographic_variant(p_ndg, ndg_variants)

      shiny::updateSelectInput(
        session,
        "ndg_variant",
        selected = detected_ndg_variant
      )

      init$destroy()
    },
    priority = 10
    )

    # renders ----

    output$non_demographic_adjustment_table <- gt::render_gt({
      mod_non_demographic_adjustment_table(non_demographic_adjustment())
    })

  })

}
