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
          shiny::selectInput(ns("baseline"), "Baseline Year", choices = c("2018/19" = 201819))
        ),
        bs4Dash::box(
          title = "Peers (from NHS Trust Peer Finder Tool)",
          width = 12,
          gt::gt_output(ns("peers_list"))
        )
      ),
      bs4Dash::box(
        title = "Map of Selected Provider and Peers",
        width = 6,
        leaflet::leafletOutput(ns("providers_map"), height = "730px")
      )
    )
  )
}


mod_home_peers_table <- function(selected_peers) {
  selected_peers |>
    sf::st_drop_geometry() |>
    dplyr::filter(.data$is_peer) |>
    dplyr::select("ODS Code" = .data$org_id, "Trust" = .data$name) |>
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
mod_home_server <- function(id, providers, peers) {
  shiny::moduleServer(id, function(input, output, session) {
    provider_locations <- sf::read_sf(app_sys("app", "data", "provider_locations.geojson"))

    shiny::observe({
      shiny::updateSelectInput(session, "provider", choices = shiny::req(providers))
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

    # return reactive
    shiny::reactive({
      list(
        provider = input$provider,
        baseline = input$baseline
      )
    })
  })
}
