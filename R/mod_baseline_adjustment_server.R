#' baseline_adjustment Server Functions
#'
#' @noRd
mod_baseline_adjustment_server <- function(id, params) {
  mod_reasons_server(shiny::NS(id, "reasons"), params, "baseline_adjustment")

  specialties <- get_lookups()[["specialties"]]

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactives ----
    baseline_data <- shiny::reactive({
      p <- shiny::isolate(params)

      get_baseline_data(p$dataset, p$start_year) |>
        dplyr::left_join(specialties, by = dplyr::join_by("tretspef")) |>
        dplyr::mutate(
          dplyr::across(
            c("tretspef", "specialty"),
            \(.x) ifelse(.data[["activity_type"]] == "aae", "Other", .x)
          )
        ) |>
        dplyr::select(
          "activity_type",
          "group",
          "tretspef",
          "specialty",
          "count"
        )
    })

    # observers ----
    shiny::observe({
      p <- shiny::isolate(params)

      df <- shiny::req(baseline_data()) |>
        dplyr::mutate(
          row_id = glue::glue("{activity_type}_{group}_{tretspef}"),
          adjustment_id = glue::glue("adjustment_{row_id}"),
          param_id = glue::glue("param_{row_id}")
        )

      # create the adjustment sliders and their observers
      df[["adjustment"]] <- purrr::pmap(
        df,
        \(
          activity_type,
          group,
          tretspef,
          adjustment_id,
          count,
          ...
        ) {
          v <- purrr::pluck(
            p[["baseline_adjustment"]],
            activity_type,
            group,
            tretspef
          )

          # observe the input
          shiny::observe({
            i <- shiny::req(input[[adjustment_id]])

            params[["baseline_adjustment"]][[activity_type]][[group]][[
              tretspef
            ]] <- if (i != 0) {
              1 + i / count
            }
          }) |>
            shiny::bindEvent(input[[adjustment_id]])

          # return the input
          shiny::sliderInput(
            ns(adjustment_id),
            label = NULL,
            min = -count,
            max = 2 * count,
            value = ((v %||% 1) - 1) * count,
            step = 1
          ) |>
            as.character() |>
            gt::html()
        }
      )

      # create the param value outputs and their renderers
      df[["param"]] <- purrr::pmap(df, \(param_id, adjustment_id, count, ...) {
        output[[param_id]] <- shiny::renderText({
          i <- shiny::req(input[[adjustment_id]])
          if (i == 0) {
            return("-")
          }
          v <- 1 + i / count
          scales::number(v, 1e-3)
        }) |>
          shiny::bindEvent(input[[adjustment_id]])

        shiny::textOutput(ns(param_id)) |>
          as.character() |>
          gt::html()
      })

      # render each table
      df |>
        dplyr::nest_by(.data[["activity_type"]], .data[["group"]]) |>
        purrr::pmap(\(activity_type, group, data) {
          ix <- paste(activity_type, group, sep = "_")
          output[[ix]] <- shiny::renderUI({
            data |>
              dplyr::select("specialty", "count", "adjustment", "param") |>
              gt::gt(rowname_col = "specialty") |>
              gt::cols_label(
                count ~ "Baseline Count",
                adjustment ~ "Adjustment",
                param ~ "Relative Change"
              ) |>
              gt::fmt_number("count", decimals = 0) |>
              gt::tab_options(table.width = gt::pct(100)) |>
              gt::as_raw_html()
          })
        })
    }) |>
      shiny::bindEvent(baseline_data(), once = TRUE)

    shiny::observe({
      shiny::req(baseline_data())

      shinyjs::toggle(
        "download_baseline",
        condition = nrow(baseline_data()) > 0
      )
    }) |>
      shiny::bindEvent(baseline_data())

    output$download_baseline <- shiny::downloadHandler(
      \() paste0(params[["dataset"]], "_baseline.csv"),
      \(filename) {
        baseline_data() |>
          dplyr::select(
            "activity_type",
            "group",
            "tretspef",
            "specialty",
            "count"
          ) |>
          readr::write_csv(filename)
      },
      "text/csv"
    )
  })
}
