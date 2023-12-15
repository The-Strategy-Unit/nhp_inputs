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

    # reactives ----
    baseline_counts <- shiny::reactive({
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))

      glue::glue("{dataset}/baseline_data.rds") |>
        load_rds_from_adls() |>
        purrr::pluck(year) |>
        tidyr::nest(.by = c("activity_type", "group")) |>
        dplyr::mutate(
          dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))
        ) |>
        tidyr::nest(.by = c("activity_type")) |>
        dplyr::mutate(
          dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))
        ) |>
        tibble::deframe() |>
        purrr::modify_at("aae", purrr::map, unname)
    })

    # observers ----

    # when the module initialy loads, run this observer and update the UI with any values that were loaded into params
    init <- shiny::observe({
      shiny::isolate({
        p <- params$baseline_adjustment
      })

      purrr::pwalk(specs, \(at, g, code, id, ...) {
        include_id <- glue::glue("include_{id}")
        param_id <- glue::glue("param_{id}")

        ix <- c(at, if (at != "aae") g, code)

        shiny::isolate({
          bc <- baseline_counts() |>
            purrr::pluck(!!!ix) |>
            shiny::req()
        })

        # get the new param value
        v <- purrr::pluck(p, !!!ix)

        shiny::updateCheckboxInput(session, include_id, value = !is.null(v))
        shiny::updateNumericInput(session, param_id, value = round(((v %||% 1) - 1) * bc))
      })

      init$destroy()
    })

    # iterate over all of the rows in specs, and create an observer for that input
    specs |>
      purrr::pwalk(\(code, at, g, id, ...) {
        include_id <- glue::glue("include_{id}")
        param_id <- glue::glue("param_{id}")

        ix <- c(at, if (at != "aae") g, code)

        bc <- baseline_counts() |>
          purrr::pluck(!!!ix)

        if (is.null(bc)) {
          shinyjs::disable(include_id)
          return()
        }

        shiny::observe({
          i <- input[[include_id]]
          shinyjs::toggleState(param_id, i)

          if (at == "aae") {
            params[["baseline_adjustment"]][[at]][[code]] <- if (i) 1 + input[[param_id]] / bc
          } else {
            params[["baseline_adjustment"]][[at]][[g]][[code]] <- if (i) 1 + input[[param_id]] / bc
          }
        }) |>
          shiny::bindEvent(input[[include_id]], input[[param_id]])
      })
  })
}
