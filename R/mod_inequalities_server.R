#' inequalities Server Functions
#'
#' @noRd
mod_inequalities_server <- function(id, inequalities_data, params) {
  # TODO: load values from params
  # TODO: dynamically generate accordions given scheme's data

  mod_reasons_server(
    shiny::NS(id, "reasons"),
    params,
    "inequalities"
  )

  shiny::moduleServer(id, function(input, output, session) {
    inequalities_data_filtered <- shiny::reactive({
      # nolint start: object_usage_linter
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))
      # nolint end

      inequalities_data |>
        dplyr::filter(
          .data[["provider"]] == .env[["dataset"]],
          .data[["fyear"]] == .env[["year"]],
          stringr::str_extract(sushrg_trimmed, "\\w{2}") == "LB"
        ) |>
        dplyr::select(
          "fyear",
          "provider",
          tidyselect::starts_with("sushrg"),
          tidyselect::everything()
        )
    })

    output$btn_download <- downloadHandler(
      filename = function() {
        paste0("data-", Sys.Date(), ".csv") # TODO: better filename
      },
      content = function(file) {
        inequalities_data_filtered() |>
          dplyr::select(-c(pvalue, slope, intercept, fitted_line)) |>
          write.csv(file)
      }
    )

    shiny::observe({
      shiny::updateSelectInput(inputId = "option_lb42", selected = "zero_sum")
      shiny::updateSelectInput(inputId = "option_lb72", selected = "zero_sum")
    }) |>
      shiny::bindEvent(input$btn_lb)

    # shiny::observe({
    #   if (input$option_lb42 == "zero_sum" & input$option_lb72 == "zero_sum") {
    #     shinyjs::disable("btn_lb")
    #   } else {
    #     shinyjs::enable("btn_lb")
    #   }
    # })

    output$plot_lb42 <- shiny::renderPlot(
      {
        mod_plot_rate_by_quintile(
          inequalities_data_filtered(),
          "LB42",
          input$option_lb42
        )
      },
      height = 100
    )

    output$plot_lb72 <- shiny::renderPlot(
      {
        mod_plot_rate_by_quintile(
          inequalities_data_filtered(),
          "LB72",
          input$option_lb72
        )
      },
      height = 100
    )
  })
}
