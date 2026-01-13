#' inequalities Server Functions
#'
#' @noRd
mod_inequalities_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_reasons_server(shiny::NS(id, "reasons"), params, "inequalities")

    # This is the data for each HRG split by IMD for the selector provider
    # load_inequalities_data() is pulling from Azure so might take some time
    provider_inequalities <- shiny::reactive({
      dataset <- shiny::req(params$dataset) # nolint: object_usage_linter

      load_inequalities_data() |>
        dplyr::filter(
          .data[["provider"]] == .env[["dataset"]]
        )
    })

    # hrg is used to track the current choice selections in table form
    hrg <- shiny::reactiveValues(
      selections = NULL
    )

    # Initialisation
    init <- shiny::observe(
      {
        # Wait for data to be available
        shiny::req(provider_inequalities())

        hrg$selections <- initialise_hrg_table(provider_inequalities(), params)

        # Destroy the observer so it only runs once
        init$destroy()
      },
      priority = -1 # Low priority to ensure other reactives are ready
    )

    # "Set all to zero sum" button
    shiny::observeEvent(input$set_all_zero_sum, {
      shiny::req(hrg$selections)
      hrg$selections$choice <- "Zero-sum"
    })

    # "Clear all" button
    shiny::observeEvent(input$clear_all, {
      shiny::req(hrg$selections)
      hrg$selections$choice <- "No change"
    })

    output$hrg_table <- DT::renderDataTable({
      shiny::req(hrg$selections)

      choice_options <- unname(get_inequality_choice_mappings())

      # Create the dropdown HTML for each row
      dropdown_html <- purrr::map_chr(
        seq_len(nrow(hrg$selections)),
        function(i) {
          current_choice <- hrg$selections$choice[i]
          options_html <- glue::glue_collapse(
            purrr::map_chr(choice_options, function(option) {
              selected <- if (option == current_choice) "selected" else ""
              glue::glue(
                "<option value='{option}' {selected}>{option}</option>"
              )
            })
          )
          glue::glue(
            "<select class='choice-select' data-row='{i}'>{options_html}</select>"
          )
        }
      )

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
    shiny::observeEvent(input$choice_changed, {
      shiny::req(hrg$selections)

      row_index <- input$choice_changed$row

      # Update the data
      hrg$selections$choice[row_index] <- input$choice_changed$value
    })

    # Download inequalities data
    output$download_inequalities <- shiny::downloadHandler(
      filename = \() glue::glue("{params[['dataset']]}_inequalities.csv"),
      content = \(file) {
        readr::write_csv(provider_inequalities(), file)
      }
    )

    shiny::observe({
      shiny::req(hrg$selections)

      params$inequalities <-
        hrg$selections |>
        dplyr::filter(.data$choice != "No change") |>
        dplyr::mutate(
          choice = inequality_choices_to_snake(.data$choice)
        ) |>
        dplyr::group_by(.data$choice) |>
        dplyr::summarise(hrg_codes = list(.data$hrg_code)) |>
        tibble::deframe() |>
        purrr::map(I) # Forces any single values to stay in a list (asis)
    })
  })
}
