#' baseline_adjustment Server Functions
#'
#' @noRd
mod_baseline_adjustment_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "baseline_adjustment")

  shiny::moduleServer(id, function(input, output, session) {
    # static data ----

    # creates a table containing all of the options shown in the baseline adjustment page, including the input id
    # for each slider
    specs <- rtt_specialties() |>
      dplyr::mutate(sanitized_code = sanitize_input_name(.data[["code"]])) |>
      dplyr::cross_join(
        dplyr::bind_rows(
          ip = tibble::tibble(g = c("elective", "non-elective", "maternity")),
          op = tibble::tibble(g = c("first", "followup", "procedure")),
          .id = "at"
        )
      ) |>
      dplyr::filter(.data[["g"]] != "maternity" | .data[["code"]] == "Other (Medical)") |>
      dplyr::bind_rows(
        tibble::tibble(
          at = "aae",
          g = "-",
          code = c("ambulance", "walk-in")
        ) |>
          dplyr::mutate(
            dplyr::across(
              "code",
              .fns = c(
                specialty = snakecase::to_title_case,
                sanitized_code = sanitize_input_name
              ),
              .names = "{.fn}"
            )
          )
      ) |>
      dplyr::mutate(id = glue::glue("{at}_{g}_{sanitized_code}"))

    # observers ----

    # when the module initialy loads, run this observer and update the UI with any values that were loaded into params
    init <- shiny::observe({
      p <- params$baseline_adjustment

      purrr::pwalk(specs, \(at, g, code, id, ...) {
        include_id <- glue::glue("include_{id}")
        param_id <- glue::glue("param_{id}")

        # get the new param value
        v <- if (at == "aae") {
          p[[at]][[code]]
        } else {
          p[[at]][[g]][[code]]
        }

        shiny::updateCheckboxInput(session, include_id, value = !is.null(v))
        shiny::updateSliderInput(session, param_id, value = v %||% mod_baseline_adjustment_default_slider_values)
      })

      init$destroy()
    })

    # iterate over all of the rows in specs, and create an observer for that input
    specs |>
      purrr::pwalk(\(code, at, g, id, ...) {
        include_id <- glue::glue("include_{id}")
        param_id <- glue::glue("param_{id}")

        shiny::observe({
          i <- input[[include_id]]
          shinyjs::toggleState(param_id, i)

          if (at == "aae") {
            params[["baseline_adjustment"]][[at]][[code]] <- if (i) input[[param_id]]
          } else {
            params[["baseline_adjustment"]][[at]][[g]][[code]] <- if (i) input[[param_id]]
          }
        }) |>
          shiny::bindEvent(input[[include_id]], input[[param_id]])
      })
  })
}
