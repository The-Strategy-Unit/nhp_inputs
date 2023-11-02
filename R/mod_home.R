#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::h1("NHP Model Inputs"),
    shiny::fluidRow(
      col_4(
        bs4Dash::box(
          collapsible = FALSE,
          headerBorder = FALSE,
          width = 12,
          md_file_to_html("app", "text", "home.md")
        )
      ),
      col_4(
        bs4Dash::box(
          title = "Select Provider and Baseline",
          collapsible = FALSE,
          width = 12,
          shiny::selectInput(ns("cohort"), "Cohort", c("Current", "All Other Providers")),
          shiny::selectInput(ns("dataset"), "Provider", choices = NULL, selectize = TRUE),
          shiny::selectInput(ns("start_year"), "Baseline Year", choices = c("2019" = 201920, "2018" = 201819)),
          shiny::sliderInput(ns("end_year"), "Model Year", min = 0, max = 19, value = 0, sep = "")
        ),
        bs4Dash::box(
          title = "Scenario",
          collapsible = FALSE,
          width = 12,
          shinyjs::disabled(
            shiny::radioButtons(
              ns("scenario_type"),
              NULL,
              c(
                "Create new from scratch",
                "Create new from existing",
                "Edit existing"
              ),
              inline = TRUE
            )
          ),
          shinyjs::hidden(
            shiny::selectInput(
              ns("previous_scenario"),
              "Previous Scenario",
              NULL
            )
          ),
          shiny::textInput(ns("scenario"), "Name"),
          shiny::textOutput(ns("status")),
          shinyjs::disabled(
            shiny::actionButton(ns("start"), "Start")
          )
        ),
        bs4Dash::box(
          title = "Advanced Options",
          width = 12,
          collapsed = TRUE,
          shiny::numericInput(ns("seed"), "Seed", sample(1:100000, 1)),
          shiny::selectInput(ns("model_runs"), "Model Runs", choices = c(256, 512, 1024), selected = 256)
        ),
        bs4Dash::box(
          title = "Upload Previous Set of Parameters",
          width = 12,
          collapsed = TRUE,
          shiny::fileInput(ns("param_upload"), "Upload")
        )
      ),
      col_4(
        bs4Dash::box(
          title = "Map of Selected Provider and Peers",
          width = 12,
          shinycssloaders::withSpinner(
            leaflet::leafletOutput(ns("providers_map"), height = "730px")
          )
        ),
        bs4Dash::box(
          title = "Peers (from NHS Trust Peer Finder Tool)",
          width = 12,
          collapsed = TRUE,
          shinycssloaders::withSpinner(
            gt::gt_output(ns("peers_list"))
          )
        )
      )
    )
  )
}


mod_home_peers_table <- function(selected_peers) {
  selected_peers |>
    sf::st_drop_geometry() |>
    dplyr::filter(.data$is_peer) |>
    dplyr::select("ODS Code" = "org_id", "Trust" = "name") |>
    gt::gt()
}

mod_home_providers_map <- function(selected_peers) {
  peer_marker <- leaflet::makeAwesomeIcon(icon = "medkit", library = "fa", markerColor = "blue")
  provider_marker <- leaflet::makeAwesomeIcon(icon = "medkit", library = "fa", markerColor = "orange")

  selected_peers |>
    leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addAwesomeMarkers(
      data = dplyr::filter(selected_peers, .data$is_peer),
      icon = peer_marker,
      popup = ~name
    ) |>
    leaflet::addAwesomeMarkers(
      data = dplyr::filter(selected_peers, !.data$is_peer),
      icon = provider_marker,
      popup = ~name
    )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, providers, params) {
  shiny::moduleServer(id, function(input, output, session) {
    peers <- load_rds_from_adls("peers.rds")
    nhp_current_cohort <- load_rds_from_adls("nhp_current_cohort.rds")

    all_providers <- jsonlite::read_json(app_sys("app", "data", "all_providers.json"), simplifyVector = TRUE)

    provider_locations <- sf::read_sf(app_sys("app", "data", "provider_locations.geojson"))

    selected_providers <- shiny::reactive({
      g <- session$groups

      cohort <- shiny::req(input$cohort)

      p <- if (cohort == "Current") {
        nhp_current_cohort
      } else if (cohort == "All Other Providers") {
        setdiff(all_providers, nhp_current_cohort)
      } else {
        stop("unrecognised option for cohort dropdown")
      }

      if (!(is.null(g) || any(c("nhp_devs", "nhp_power_users") %in% g))) {
        a <- g |>
          stringr::str_subset("^nhp_provider_") |>
          stringr::str_remove("^nhp_provider_")
        p <- intersect(p, a)
      }

      p <- providers[providers %in% p]
    })

    shiny::observe({
      shiny::updateSelectInput(session, "dataset", choices = selected_providers())
    })

    shiny::observe({
      x <- as.numeric(stringr::str_sub(input$start_year, 1, 4))

      shiny::updateSliderInput(session, "end_year", min = x + 1, max = x + 20, value = x + 15)
    })

    selected_peers <- shiny::reactive({
      p <- shiny::req(input$dataset)

      provider_locations |>
        dplyr::semi_join(
          peers |>
            dplyr::filter(.data$procode == p),
          by = c("org_id" = "peer")
        ) |>
        dplyr::mutate(is_peer = .data$org_id != p)
    })

    output$peers_list <- gt::render_gt({
      mod_home_peers_table(selected_peers())
    })

    output$providers_map <- leaflet::renderLeaflet({
      mod_home_providers_map(selected_peers())
    })

    shiny::observe({
      p <- session$userData$params <- load_params(input$param_upload$datapath)

      if (p$dataset == "synthetic") {
        cat("skipping changing the dataset\n")
      } else {
        shiny::updateSelectInput(session, "dataset", selected = p$dataset)
      }

      shiny::updateTextInput(session, "scenario", value = p$scenario)

      y <- p$start_year * 100 + p$start_year %% 100 + 1
      shiny::updateSelectInput(session, "start_year", selected = y)
      shiny::updateNumericInput(session, "end_year", value = p$end_year)
      shiny::updateNumericInput(session, "seed", value = p$seed)
      shiny::updateSelectInput(session, "model_runs", selected = p$model_runs)
    }) |>
      shiny::bindEvent(input$param_upload)

    shiny::observe({
      ds <- shiny::req(input$dataset)

      saved_params <- params_path(session$user, ds) |>
        dir(pattern = "*.json") |>
        stringr::str_remove("\\.json$")

      shiny::updateRadioButtons(
        session,
        "scenario_type",
        selected = "Create new from scratch"
      )
      shiny::updateTextInput(
        session,
        "scenario",
        value = ""
      )
      shinyjs::toggleState("scenario_type", condition = length(saved_params) > 0)

      shiny::updateSelectInput(
        session,
        "previous_scenario",
        choices = saved_params
      )
    }) |>
      shiny::bindEvent(
        input$dataset
      )

    shiny::observe({
      if (input$scenario_type == "Create new from scratch") {
        shinyjs::show("scenario")
        shinyjs::hide("previous_scenario")
      } else if (input$scenario_type == "Create new from existing") {
        shinyjs::show("scenario")
        shinyjs::show("previous_scenario")
      } else if (input$scenario_type == "Edit existing") {
        shinyjs::hide("scenario")
        shinyjs::show("previous_scenario")
        shiny::updateTextInput(
          session,
          "scenario",
          value = input$previous_scenario
        )
      }
    }) |>
      shiny::bindEvent(
        input$scenario_type,
        input$previous_scenario
      )

    shiny::observe({
      if (input$scenario_type == "Create new from scratch") {
        session$userData$params <- NULL
        return()
      }

      file <- params_filename(
        session$user,
        input$dataset,
        input$previous_scenario
      )

      # prevents bug where the dataset is changed before the previous scenarios are updated
      shiny::req(file.exists(file))

      p <- session$userData$params <- load_params(file)

      y <- p$start_year * 100 + p$start_year %% 100 + 1
      shiny::updateSelectInput(session, "start_year", selected = y)
      shiny::updateNumericInput(session, "end_year", value = p$end_year)
      shiny::updateNumericInput(session, "seed", value = p$seed)
      shiny::updateSelectInput(session, "model_runs", selected = p$model_runs)
    }) |>
      shiny::bindEvent(
        input$dataset,
        input$scenario_type,
        input$previous_scenario
      )

    # the scenario must have some validation applied to it - the next few chunks handle this
    # we use the status output to be the placeholder for the validation text, this is used in the
    # main UI to control the visibility of the items in the panel (`output.status === 'TRUE'`)
    # we observe the validation to make sure that we have TRUE returned, and then hide the output
    # (we only want to show the status output if there are validation errors)
    scenario_validation <- shiny::reactive({
      s <- input$scenario
      f <- params_filename(session$user, input$dataset, input$scenario)

      shiny::validate(
        shiny::need(
          s != "",
          "Scenario must be completed in order to proceed",
          "Scenario"
        ),
        shiny::need(
          !stringr::str_detect(s, "[^a-zA-Z0-9\\-]"),
          "Scenario can only container letters, numbers, and - characters",
          "Scenario"
        ),
        shiny::need(
          input$scenario_type == "Edit existing" || !file.exists(f),
          "Scenario already exists",
          "Scenario"
        )
      )

      TRUE
    })

    output$status <- shiny::renderText(scenario_validation())

    shiny::observe({
      x <- tryCatch(scenario_validation(), error = \(...) FALSE)
      shinyjs::toggle("status", condition = !x)
      shinyjs::toggleState("start", condition = x)
    })

    # update the params items with the selections
    shiny::observe({
      shiny::req(scenario_validation())

      params$dataset <- input$dataset
      params$scenario <- input$scenario
      params$seed <- input$seed
      params$model_runs <- as.numeric(input$model_runs)
      params$start_year <- input$start_year
      params$end_year <- input$end_year
    })

    # return the start input so we can observe it in the main server
    shiny::reactive({
      input$start
    })
  })
}
