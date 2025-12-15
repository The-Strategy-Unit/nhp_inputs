#' inequalities Server Functions
#'
#' @noRd
mod_inequalities_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_reasons_server(shiny::NS(id, "reasons"), params, "inequalities")

    inequalities_data <- shiny::reactive({
      dataset <- shiny::req(params$dataset) # nolint: object_usage_linter

      load_inequalities_data() |>
        dplyr::filter(
          .data[["provider"]] == .env[["dataset"]]
        )
    })

    # Initialize reactiveValues with NULL
    hrg <- reactiveValues(
      selections = NULL
    )

    # Initialise data once inequalities_data is available
    observe({
      req(inequalities_data())

      if (is.null(hrg$selections)) {
        hrg$selections <- tibble::tibble(
          hrg_code = unique(inequalities_data()$sushrg_trimmed),
          choice = "No change"
        )
      }
    })

    # Handle "Set all to zero sum" button
    observeEvent(input$set_all_zero_sum, {
      hrg$selections$choice <- "Zero-sum"
    })

    # Handle "Clear all" button
    observeEvent(input$clear_all, {
      hrg$selections$choice <- "No change"
    })

    output$hrg_table <- DT::renderDataTable({
      # Create dropdown options for Choice column
      choice_options <- c("No change", "Zero-sum", "Level up", "Level down")

      # Create the dropdown HTML for each row
      dropdown_html <- sapply(seq_len(nrow(hrg$selections)), function(i) {
        current_choice <- hrg$selections$choice[i]
        options_html <- glue::glue_collapse(
          sapply(choice_options, function(option) {
            selected <- if (option == current_choice) "selected" else ""
            glue::glue("<option value='{option}' {selected}>{option}</option>")
          })
        )
        glue::glue(
          "<select class='choice-select' data-row='{i}'>{options_html}</select>"
        )
      })

      # Replace the choice column with dropdown HTML
      display_data <- hrg$selections
      display_data$choice <- dropdown_html

      DT::datatable(
        display_data,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        filter = "top",
        options = list(
          pageLength = 25,
          searching = FALSE,
          ordering = TRUE,
          info = TRUE
        ),
        callback = DT::JS(glue::glue(
          "
  table.on('change', '.choice-select', function() {{
    var row = $(this).data('row');
    var value = $(this).val();
    Shiny.setInputValue('{ns('choice_changed')}', {{row: row, value: value}}, {{priority: 'event'}});
  }});
"
        ))
      )
    })

    # Handle dropdown changes
    observeEvent(input$choice_changed, {
      row_index <- input$choice_changed$row
      new_value <- input$choice_changed$value

      # Update the data
      hrg$selections$choice[row_index] <- new_value
    })

    output$download_inequalities <- shiny::downloadHandler(
      filename = \() glue::glue("{params[['dataset']]}_inequalities.csv"),
      content = \(file) {
        readr::write_csv(inequalities_data(), file)
      }
    )
  })
}
