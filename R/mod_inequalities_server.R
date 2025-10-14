#' inequalities Server Functions
#'
#' @noRd
mod_inequalities_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {


    inequalities_data <- shiny::reactive({

      dataset <- shiny::req(params$dataset)

      load_inequalities_data() |>
        dplyr::filter(
          .data[["provider"]] == .env[["dataset"]]
        )

    })

    output$download_inequalities <- shiny::downloadHandler(
      filename = \() glue::glue("{params[['dataset']]}_inequalities.csv"),
      content = \(file) {
        readr::write_csv(inequalities_data(), file)
      }
    )

  })
}
