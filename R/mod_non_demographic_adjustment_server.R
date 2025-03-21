#' Non Demographic Server Functions
#'
#' @noRd
mod_non_demographic_adjustment_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "non-demographic_adjustment")

  shiny::moduleServer(id, function(input, output, session) {
    ndg_variants <- app_sys("app", "data", "ndg_variants.json") |>
      jsonlite::read_json(simplifyVector = TRUE) |>
      purrr::keep_at(c("variant_2", "variant_3"))

    # reactives ----

    non_demographic_adjustment <- shiny::reactive({
      v <- shiny::req(input$ndg_variant)
      ndg_variants[[v]]  # list including variant, type and values
    })

    # observers ----

    shiny::observe({
      params[["non-demographic_adjustment"]] <- non_demographic_adjustment()
    }) |>
      shiny::bindEvent(non_demographic_adjustment())

    init <- shiny::observe(
      {
        p_ndg_variant <- shiny::isolate({
          params[["non-demographic_adjustment"]][["variant"]]
        })

        shiny::updateSelectInput(
          session,
          "ndg_variant",
          selected = p_ndg_variant
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
