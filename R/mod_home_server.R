#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, providers, filename) {
  shiny::moduleServer(id, function(input, output, session) {
    # reactives ----

    params <- shiny::reactiveValues()

    # observers ----

    # load the selected params
    # if the user chooses to create new from scratch, we use the default parameters file
    # otherwise, load the values for the scenario the user selected
    init <- shiny::observe({
      file <- filename()

      # make sure the file exists before loading it
      shiny::req(file.exists(file))
      p <- load_params(file)

      # copy the loaded params into params
      purrr::walk(names(p), \(i) params[[i]] <- p[[i]])

      init$destroy()
    })

    # renders ----
    output$model_options <- gt::render_gt({
      p <- params |>
        shiny::reactiveValuesToList() |>
        purrr::keep(is.atomic) |>
        tibble::enframe()

      gt::gt(p, "name")
    })

    # return ----
    params
  })
}
