mod_run_model_fix_params <- function(p, user) {
  # combine efficiences/activity avoidance items
  p[["efficiencies"]] <- list(
    ip = c(
      p[["efficiencies|mean_los"]]$ip,
      p[["efficiencies|aec"]]$ip,
      p[["efficiencies|preop"]]$ip,
      p[["efficiencies|bads"]]$ip
    ),
    op = c(
      p[["efficiencies|ctt"]]$op
    )
  )

  p[["activity_avoidance"]]$op <- c(
    p[["activity_avoidance|c2c"]]$op,
    p[["activity_avoidance|f2f"]]$op
  )

  p[["activity_avoidance"]]$aae <- c(
    p[["activity_avoidance|fa"]]$aae,
    p[["activity_avoidance|lbs"]]$aae,
    p[["activity_avoidance|lcd"]]$aae
  )

  # remove the items
  p[["efficiencies|mean_los"]] <- NULL
  p[["efficiencies|aec"]] <- NULL
  p[["efficiencies|preop"]] <- NULL
  p[["efficiencies|bads"]] <- NULL
  p[["efficiencies|ctt"]] <- NULL
  p[["activity_avoidance|c2c"]] <- NULL
  p[["activity_avoidance|f2f"]] <- NULL
  p[["activity_avoidance|fa"]] <- NULL
  p[["activity_avoidance|lbs"]] <- NULL
  p[["activity_avoidance|lcd"]] <- NULL

  p$start_year <- as.integer(p$start_year)

  # generate an id based on the dataset, scenario, and a hash of the params
  # make sure to add the user after creating the hash
  # the id must be at most 63 characters long, and must match the regex:
  #   '[a-z0-9]([-a-z0-9]*[a-z0-9])?'
  scenario_sanitized <- p$scenario |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "-")
  hash <- digest::digest(p, "crc32", serialize = TRUE)

  p$id <- glue::glue("{p$dataset}-{scenario_sanitized}") |>
    stringr::str_sub(1, 63 - stringr::str_length(hash)) |>
    stringr::str_to_lower() |>
    paste0("-", hash)

  p$user <- user
  # reorder the params
  p_order <- c(
    "id",
    "user",
    "dataset",
    "scenario",
    "seed",
    "model_runs",
    "start_year",
    "end_year",
    "app_version",
    "create_datetime",
    "demographic_factors",
    "health_status_adjustment",
    "life_expectancy",
    "expat",
    "repat_local",
    "repat_nonlocal",
    "baseline_adjustment",
    "waiting_list_adjustment",
    "non-demographic_adjustment",
    "activity_avoidance",
    "efficiencies",
    "bed_occupancy",
    "theatres"
  )

  # make sure to only select items that exist in the params
  p_order <- p_order[p_order %in% names(p)]
  # add in any items in the params that aren't in the order at the end
  p_order <- c(p_order, setdiff(names(p), p_order))

  p[p_order]
}

mod_run_model_submit <- function(params) {
  api_uri <- Sys.getenv("NHP_RUN_MODEL_API_URI")

  req <- httr::POST(api_uri, body = params, encode = "json")

  httr::status_code(req)
}

#' run_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_run_model_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::box(
      title = "Run Model",
      shiny::actionButton(ns("submit"), "Submit Model Run"),
      shiny::textOutput(ns("status"))
    ),
    bs4Dash::box(
      title = "Params",
      shiny::verbatimTextOutput(ns("params_json"))
    )
  )
}

#' run_model Server Functions
#'
#' @noRd
mod_run_model_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    status <- shiny::reactiveVal("Waiting")

    fixed_params <- shiny::reactive({
      params |>
        shiny::reactiveValuesToList() |>
        mod_run_model_fix_params(session$user)
    })

    output$status <- shiny::renderText(status())

    shiny::observe({
      shinyjs::disable("submit")
      results <- mod_run_model_submit(fixed_params())

      if (results == 200) {
        status("Success")
      } else {
        status(paste("Error:", results))
      }
    }) |>
      shiny::bindEvent(input$submit)

    output$params_json <- shiny::renderPrint({
      jsonlite::toJSON(fixed_params(), pretty = TRUE, auto_unbox = TRUE)
    })
  })
}
