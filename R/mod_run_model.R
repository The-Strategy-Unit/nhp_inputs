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

  # some of the items in our params will be lists of length 0.
  # jsonlite will serialize these as empty arrays
  #   toJSON(list()) == "[]"
  # we need to have these serialize as empty objects, so we need to replace
  # these with NULL's as
  #   toJSON(NULL) == "{}"
  recursive_nullify <- function(.x) {
    for (i in names(.x)) {
      if (length(.x[[i]]) == 0) {
        .x[i] <- list(NULL)
      } else {
        .x[[i]] <- recursive_nullify(.x[[i]])
      }
    }
    .x
  }
  p <- recursive_nullify(p)

  # for now, hard coding in life expectancy
  p$life_expectancy <- list(
    "f" = c(
      1.8,
      1.8,
      1.8,
      1.8,
      1.8,
      1.7,
      1.8,
      1.8,
      1.7,
      1.7,
      1.7,
      1.7,
      1.6,
      1.6,
      1.5,
      1.5,
      1.5,
      1.4,
      1.4,
      1.3,
      1.3,
      1.2,
      1.1,
      1.1,
      1.1,
      1,
      0.9,
      0.9,
      0.8,
      0.8,
      0.8,
      0.7,
      0.6,
      0.6,
      0.5,
      0.1545
    ),
    "m" = c(
      2.1,
      2.1,
      2.1,
      2,
      1.9,
      2,
      1.9,
      1.9,
      1.9,
      1.9,
      1.8,
      1.8,
      1.7,
      1.7,
      1.6,
      1.6,
      1.6,
      1.5,
      1.5,
      1.4,
      1.4,
      1.3,
      1.3,
      1.3,
      1.2,
      1.1,
      1,
      0.9,
      0.9,
      0.8,
      0.8,
      0.8,
      0.7,
      0.6,
      0.6,
      0.2364
    ),
    "min_age" = 55,
    "max_age" = 90
  )

  # need to convert financial year to calendar year
  p$start_year <- as.integer(stringr::str_sub(p$start_year, 1, 4))

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
    "covid_adjustment",
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
    col_6(
      bs4Dash::box(
        title = "Run Model",
        width = 12,
        shiny::actionButton(ns("submit"), "Submit Model Run"),
        shiny::uiOutput(ns("status"))
      ),
      bs4Dash::box(
        title = "Download Params",
        width = 12,
        shiny::downloadButton(ns("download_params"), "Download params")
      )
    ),
    bs4Dash::box(
      title = "View Params",
      collapsed = TRUE,
      shiny::verbatimTextOutput(ns("params_json"))
    )
  )
}

#' run_model Server Functions
#'
#' @noRd
mod_run_model_server <- function(id, params) {
  shiny::moduleServer(id, function(input, output, session) {
    status <- shiny::reactiveVal()
    results_url <- shiny::reactiveVal()

    fixed_params <- shiny::reactive({
      shiny::req(params$scenario != "")

      params |>
        shiny::reactiveValuesToList() |>
        mod_run_model_fix_params(session$user)
    })

    output$status <- shiny::renderUI({
      s <- shiny::req(status())

      if (s == "Success") {
        shiny::tags$p(
          "Completed: ",
          shiny::tags$a(href = results_url(), "View Outputs")
        )
      } else {
        s
      }
    })

    shiny::observe({
      shiny::req(input$submit)
      shinyjs::disable("submit")
      shinyjs::hide(selector = "#sidebarItemExpanded")
      status("Please Wait...")

      p <- shiny::req(fixed_params())
      p$create_datetime <- format(Sys.time(), "%Y%m%d_%H%M%S")

      # generate the url
      ds <- p$dataset
      sc <- utils::URLencode(p$scenario)
      cd <- p$create_datetime
      uri <- Sys.getenv("NHP_OUTPUTS_URI")
      results_url(glue::glue("{uri}#/{ds}/{sc}/{cd}"))

      promises::future_promise(
        {
          httr::POST(
            Sys.getenv("NHP_API_URI"),
            path = c("api", "run_model"),
            query = list(
              app_version = Sys.getenv("NHP_APP_VERSION", "dev"),
              code = Sys.getenv("NHP_API_KEY")
            ),
            body = p,
            encode = "json"
          )
        },
        packages = character()
      ) %...>%
        (\(request) {
          response <- httr::status_code(request)
          if (response == 200) {
            status("Submitted Model Run")
          } else {
            status(paste("Error:", response))
          }
        })

      # do not return the promise
      invisible(NULL)
    }) |>
      shiny::bindEvent(input$submit)

    model_run_status_refresh <- shiny::reactiveTimer(10000)
    shiny::observe({
      # ensure the button has been pressed
      shiny::req(input$submit)

      # stop once the model run has finished
      shiny::req(status() %in% c("Modelling running", "Submitted Model Run"))

      p <- shiny::req(fixed_params())
      id <- p$id

      promises::future_promise(
        {
          httr::GET(
            Sys.getenv("NHP_API_URI"),
            path = c("api", "model_run_status", id),
            query = list(
              code = Sys.getenv("NHP_API_KEY")
            )
          )
        },
        packages = character()
      ) %...>%
        (\(request) {
          # will get a 500 error before the container is actually created
          shiny::req(httr::status_code(request) == 200)

          res <- httr::content(request)

          if (res$state == "Terminated") {
            if (res$detail_status == "Completed") {
              status("Success")
            } else {
              status("Error")
            }
          } else {
            status("Modelling running")
          }
        })

      # do not return the promise
      invisible(NULL)
    }) |>
      shiny::bindEvent(model_run_status_refresh())

    output$params_json <- shiny::renderText({
      jsonlite::toJSON(fixed_params(), pretty = TRUE, auto_unbox = TRUE)
    })

    shiny::observe({
      p <- !is.null(tryCatch(fixed_params(), error = \(...) NULL))

      shinyjs::toggleState("submit", condition = p && !input$submit)
      shinyjs::toggleState("download_params", condition = p)
    })

    output$download_params <- shiny::downloadHandler(
      filename = \() paste0(fixed_params()$id, ".json"),
      content = \(file) jsonlite::write_json(fixed_params(), file, pretty = TRUE, auto_unbox = TRUE)
    )
  })
}
