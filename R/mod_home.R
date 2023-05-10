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
      col_6(
        bs4Dash::box(
          title = "Select Provider and Baseline",
          width = 12,
          shiny::selectInput(ns("provider"), "Provider", choices = NULL),
          shiny::selectInput(ns("baseline"), "Baseline Year", choices = c("2019" = 201920, "2018" = 201819)),
          shiny::sliderInput(ns("model_year"), "Model Year", min = 0, max = 19, value = 0, sep = ""),
          shiny::textInput(ns("scenario_name"), "Scenario Name"),
          shiny::numericInput(ns("seed"), "Seed", sample(1:100000, 1)),
          shiny::selectInput(ns("model_runs"), "Model Runs", choices = c(256, 512, 1024), selected = 256)
        ),
        bs4Dash::box(
          title = "Peers (from NHS Trust Peer Finder Tool)",
          width = 12,
          shinycssloaders::withSpinner(
            gt::gt_output(ns("peers_list"))
          )
        )
      ),
      bs4Dash::box(
        title = "Map of Selected Provider and Peers",
        width = 6,
        shinycssloaders::withSpinner(
          leaflet::leafletOutput(ns("providers_map"), height = "730px")
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
    leaflet::addProviderTiles("Stamen.TonerLite") |>
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

    provider_locations <- sf::read_sf(app_sys("app", "data", "provider_locations.geojson"))

    shiny::observe({
      choices <- providers[providers %in% nhp_current_cohort]
      shiny::updateSelectInput(session, "provider", choices = choices)
    })

    shiny::observe({
      x <- as.numeric(stringr::str_sub(input$baseline, 1, 4))

      shiny::updateSliderInput(session, "model_year", min = x + 1, max = x + 20)
    })

    selected_peers <- shiny::reactive({
      p <- shiny::req(input$provider)

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
      # TODO: need to provide inputs for all of the items below
      params$dataset <- input$provider
      params$scenario <- input$scenario_name
      params$seed <- input$seed
      params$model_runs <- input$model_runs
      params$start_year <- input$baseline
      params$end_year <- input$model_year
      params$create_datetime <- format(Sys.time(), "%Y%m%d_%H%M%S")
    })
  })
}
