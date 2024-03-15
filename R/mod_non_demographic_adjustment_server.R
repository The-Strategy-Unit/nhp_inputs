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

    # Show variant dropdown and rationale boxes
    shiny::observe({
      can_select_variant <-
        is_local() || any(c("nhp_devs", "nhp_run_model") %in% session$groups)
      shinyjs::toggle("ndg_variant_box", condition = can_select_variant)
    })

    shiny::observe({
      params[["non-demographic_adjustment"]] <- non_demographic_adjustment()
    }) |>
      shiny::bindEvent(non_demographic_adjustment())

    init <- shiny::observe({

      p <- shiny::isolate({
        params[["non-demographic_adjustment"]]
      })

      current_ndg_values <- unlist(p)

      if (!is.null(current_ndg_values)) {

        current_ndg_values <- current_ndg_values |> round(4)
        ndg_variant_sets <- purrr::map(ndg_variants, \(x) round(unlist(x), 4))

        current_ndg_variant <- purrr::map(
          ndg_variant_sets,
          \(x) all(current_ndg_values == x)
        ) |>
          purrr::keep(isTRUE) |>
          names()  # e.g. "variant_2"

        shiny::updateSelectInput(
          session,
          "ndg_variant",
          selected = current_ndg_variant
        )

      }

      init$destroy()
    },
    priority = 10
    )

    # renders ----

    output$non_demographic_adjustment_table <- gt::render_gt({
      mod_non_demographic_adjustment_table(non_demographic_adjustment())
    })

    # output$ndg_variant_select <- shiny::renderUI({
    #   shiny::selectInput(
    #     inputId = ns("ndg_variant"),
    #     label = "Selection",
    #     choices = purrr::set_names(
    #       c("variant_1", "variant_2"),
    #       snakecase::to_title_case
    #     ),
    #     selected = current_ndg_variant
    #   )
    # })

  })

}
