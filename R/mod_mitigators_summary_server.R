#' mitigator_summary Server Functions
#'
#' @noRd
mod_mitigators_summary_server <- function(id, provider_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$diagnoses_table <- gt::render_gt({

      provider_data() |>
        purrr::map("age_sex") |>
        dplyr::bind_rows(.id = "strategy") |>
        dplyr::filter(fyear == max(fyear)) |>
        dplyr::summarise(total = sum(n), .by = strategy) |>
        dplyr::arrange(desc(total)) |>
        dplyr::slice(1 : 20) |>
        dplyr::mutate(strategy = snakecase::to_title_case(strategy)) |>
        gt::gt() |>
        gt::cols_label(
          "strategy" = "Mitigator",
          "total" = "Total"
        ) |>
        gt::fmt_number(
          c("total"),
          decimals = 0,
          use_seps = TRUE
        )
    })

  })
}
