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
        param_id <- glue::glue("param_{id}")

        ix <- c(at, if (at != "aae") g, code)

        shiny::isolate({
          bc <- baseline_counts() |>
            purrr::pluck(!!!ix) |>
            shiny::req()
        })

        # get the new param value
        v <- purrr::pluck(p, !!!ix)

        shiny::updateNumericInput(
          session,
          param_id,
          value = round(((v %||% 1) - 1) * bc)
        )
      })

      init$destroy()
    })

    # iterate over all of the rows in specs, and create an observer for that input
    specs |>
      purrr::pwalk(\(code, at, g, id, ...) {
        adjustment_id <- glue::glue("adjustment_{id}")
        baseline_id <- glue::glue("baseline_{id}")
        param_id <- glue::glue("param_{id}")

        ix <- c(at, if (at != "aae") g, code)

        bc <- baseline_counts() |>
          shiny::req() |>
          purrr::pluck(!!!ix)

        if (is.null(bc)) {
          shinyjs::runjs(
            glue::glue(
              "$('#{session$ns(baseline_id)}').parents('tr').remove()"
            )
          )
          return()
        }

        # update numeric input doesn't update min/max for some reason
        shinyjs::runjs(
          glue::glue(
            "$('#{session$ns(adjustment_id)}').attr('min', -{bc}).attr('max', {2 * bc});"
          )
        )

        output[[baseline_id]] <- shiny::renderText({
          scales::comma(bc)
        })

        adjustment_value <- shiny::reactive({
          i <- shiny::req(input[[adjustment_id]])

          if (!stringr::str_detect(i, "^-[0-9]")) {
            i
          }
        })

        output[[param_id]] <- shiny::renderText({
          shiny::validate(
            shiny::need(
              !is.null(input[[adjustment_id]]),
              "Invalid adjustment value, defaulting to 0"
            )
          )

          i <- shiny::req(adjustment_value())
          if (i == 0) {
            return("-")
          }
          v <- 1 + i / bc
          scales::number(v, 1e-6)
        })

        shiny::observe({
          i <- adjustment_value() %||% 0
          v <- 1 + i / bc

          if (at == "aae") {
            params[["baseline_adjustment"]][[at]][[code]] <- if (i != 0) v
          } else {
            params[["baseline_adjustment"]][[at]][[g]][[code]] <- if (i != 0) v
          }
        }) |>
          shiny::bindEvent(input[[adjustment_id]])
      })
  })
}
