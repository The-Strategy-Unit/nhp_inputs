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
      dplyr::filter(
        .data[["g"]] != "maternity" | .data[["code"]] == "Other (Medical)"
      ) |>
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
    baseline_data <- shiny::reactive({
      dataset <- shiny::req(params$dataset)
      year <- as.character(shiny::req(params$start_year))

      load_provider_data("baseline") |>
        dplyr::filter(
          .data[["fyear"]] == .env[["year"]],
          .data[["provider"]] == .env[["dataset"]]
        ) |>
        dplyr::select(
          "activity_type",
          "group",
          "tretspef",
          "n" = "count"
        )
    })

    baseline_counts <- shiny::reactive({
      baseline_data() |>
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
    init <- shiny::observe(
      {
        shiny::isolate({
          p <- params$baseline_adjustment

          bc <- baseline_counts()

          purrr::pwalk(specs, \(at, g, code, id, ...) {
            adjustment_id <- glue::glue("adjustment_{id}")

            ix <- c(at, if (at != "aae") g, code)

            bcd <- purrr::pluck(bc, !!!ix)

            if (is.null(bcd)) {
              # don't use shiny::req here, it ends up exiting the entire pwalk
              return()
            }

            # get the new param value
            v <- purrr::pluck(p, !!!ix)

            shiny::updateSliderInput(
              session,
              adjustment_id,
              min = -bcd,
              max = 2 * bcd,
              value = round(((v %||% 1) - 1) * bcd)
            )
          })
        })

        init$destroy()
      },
      priority = 10
    )

    shiny::observe({
      shiny::req(baseline_counts())

      shinyjs::toggle(
        "download_baseline",
        condition = nrow(baseline_counts()) > 0
      )
    }) |>
      shiny::bindEvent(baseline_counts())

    output$download_baseline <- shiny::downloadHandler(
      \() paste0(params[["dataset"]], "_baseline.csv"),
      \(filename) readr::write_csv(baseline_data(), filename),
      "text/csv"
    )

    # iterate over all of the rows in specs, and create an observer for that input
    specs |>
      purrr::pwalk(\(code, at, g, id, ...) {
        adjustment_id <- glue::glue("adjustment_{id}")
        baseline_id <- glue::glue("baseline_{id}")
        param_id <- glue::glue("param_{id}")

        ix <- c(at, if (at != "aae") g, code)

        bcd <- baseline_counts() |>
          purrr::pluck(!!!ix)

        if (is.null(bcd)) {
          shinyjs::runjs(
            glue::glue(
              "$('#{session$ns(baseline_id)}').parents('tr').remove()"
            )
          )
          return()
        }

        output[[baseline_id]] <- shiny::renderText({
          scales::comma(bcd)
        })

        output[[param_id]] <- shiny::renderText({
          i <- shiny::req(input[[adjustment_id]])
          if (i == 0) {
            return("-")
          }
          v <- 1 + i / bcd
          scales::number(v, 1e-6)
        })

        shiny::observe({
          i <- shiny::req(input[[adjustment_id]])
          v <- 1 + i / bcd

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
