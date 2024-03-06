"%||%" <- function(x, y) { # nolint
  if (is.null(x)) {
    y
  } else {
    x
  }
}

load_params <- function(file) {
  p <- jsonlite::read_json(file, simplifyVector = TRUE)

  # handle old non-demographic adjusment
  if (!is.null(p[["non-demographic"]][["elective"]])) {
    p[["non-demographic_adjustment"]] <- list(
      ip = list(),
      op = list(),
      aae = list()
    )
  }

  p
}

params_path <- function(user, dataset) {
  path <- file.path(
    config::get("params_data_path"),
    "params",
    user %||% ".",
    dataset
  )

  dir.create(path, FALSE, TRUE)

  path
}

params_filename <- function(user, dataset, scenario) {
  file.path(
    params_path(user, dataset),
    paste0(scenario, ".json")
  )
}

# check to see whether the app is running locally or in production
is_local <- function() {
  Sys.getenv("SHINY_PORT") == "" || !getOption("golem.app.prod", TRUE)
}

peers_table <- function(selected_peers) {
  selected_peers |>
    sf::st_drop_geometry() |>
    dplyr::filter(.data$is_peer) |>
    dplyr::select("ODS Code" = "org_id", "Trust" = "name") |>
    gt::gt()
}

providers_map <- function(selected_peers) {
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


ui_body <- function() {
  # each of the columns is created in it's own variable

  # left column contains the documentation for this module
  left_column <- shiny::column(
    width = 4,
    bs4Dash::box(
      collapsible = FALSE,
      headerBorder = FALSE,
      width = 12,
      shiny::HTML(markdown::mark_html("home.md", output = FALSE, template = FALSE))
    )
  )

  # middle column contains the inputs that the user is going to set
  middle_column <- shiny::column(
    width = 4,
    bs4Dash::box(
      title = "Select Provider and Baseline",
      collapsible = FALSE,
      width = 12,
      shiny::selectInput("cohort", "Cohort", c("Current", "All Other Providers")),
      shiny::selectInput("dataset", "Provider", choices = NULL, selectize = TRUE),
      shiny::selectInput(
        "start_year",
        "Baseline Financial Year",
        choices = c("2019/20" = 201920)
      ),
      shiny::selectInput(
        "end_year",
        "Model Financial Year",
        choices = setNames(as.character(0:21), paste(2020:2041, 21:42, sep = "/")),
        selected = "21"
      )
    ),
    bs4Dash::box(
      title = "Scenario",
      collapsible = FALSE,
      width = 12,
      shinyjs::disabled(
        shiny::radioButtons(
          "scenario_type",
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
          "previous_scenario",
          "Previous Scenario",
          NULL
        )
      ),
      shiny::textInput("scenario", "Name"),
      shiny::uiOutput("start_button")
    ),
    bs4Dash::box(
      title = "Advanced Options",
      width = 12,
      collapsed = TRUE,
      shiny::numericInput("seed", "Seed", sample(1:100000, 1)),
      shiny::selectInput("model_runs", "Model Runs", choices = c(256, 512, 1024), selected = 256),
      shinyjs::disabled(
        shiny::selectInput("app_version", "Model Version", choices = c("v1.0", "dev"))
      )
    )
  )

  # right column contains the outputs in the home module (map and peers list)
  right_column <- shiny::column(
    width = 4,
    bs4Dash::box(
      title = "Map of Selected Provider and Peers",
      width = 12,
      shinycssloaders::withSpinner(
        leaflet::leafletOutput("providers_map", height = "730px")
      )
    ),
    bs4Dash::box(
      title = "Peers (from NHS Trust Peer Finder Tool)",
      width = 12,
      collapsed = TRUE,
      shinycssloaders::withSpinner(
        gt::gt_output("peers_list")
      )
    )
  )

  # build the home page outputs
  bs4Dash::bs4DashBody(
    htmltools::h1("NHP Model Inputs"),
    shiny::fluidRow(
      left_column,
      middle_column,
      right_column
    )
  )
}

ui <- bs4Dash::bs4DashPage(
  bs4Dash::dashboardHeader(disable = TRUE),
  bs4Dash::dashboardSidebar(disable = TRUE),
  ui_body(),
  help = NULL,
  dark = NULL
)


server <- function(input, output, session) {
  # static data ----
  peers <- readRDS("peers.Rds")
  nhp_current_cohort <- readRDS("nhp_current_cohort.Rds")

  providers <- readRDS("providers.Rds")
  all_providers <- jsonlite::read_json("all_providers.json", simplifyVector = TRUE)

  provider_locations <- sf::read_sf("provider_locations.geojson")

  # reactives ----

  # each time the user connects we create a temporary file which is what is passed to the main inputs app

  tempfile_name <- shiny::reactive({
    path <- file.path(config::get("params_data_path"), "tmp")
    dir.create(path, FALSE, TRUE)
    tempfile("", tmpdir = path)
  })

  # only show the providers that a user is allowed to access
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
  }) |>
    shiny::bindEvent(input$cohort)

  # when the user changes the provider (dataset), get the list of peers for that provider
  selected_peers <- shiny::reactive({
    p <- shiny::req(input$dataset)

    provider_locations |>
      dplyr::semi_join(
        peers |>
          dplyr::filter(.data$procode == p),
        by = c("org_id" = "peer")
      ) |>
      dplyr::mutate(is_peer = .data$org_id != p)
  }) |>
    shiny::bindEvent(input$dataset)

  # the scenario must have some validation applied to it - the next few chunks handle this
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

    # scenario is valid, so return TRUE. the validate function will return an error if there are issues
    TRUE
  }) |>
    shiny::bindEvent(input$dataset, input$scenario, input$scenario_type)

  # load the selected params
  # if the user chooses to create new from scratch, we use the default parameters file
  # otherwise, load the values for the scenario the user selected
  params <- shiny::reactive({
    default_params <- "default_params.json"
    file <- if (input$scenario_type == "Create new from scratch") {
      default_params
    } else {
      params_filename(
        session$user,
        input$dataset,
        input$previous_scenario
      )
    }

    # make sure the file exists before loading it
    shiny::req(file.exists(file))
    p <- load_params(file)

    # if we use the default parameters
    if (file == default_params) {
      p$seed <- sample(1:100000, 1)
    }
    p$user <- session$user %||% "[development]"

    return(p)
  }) |>
    shiny::bindEvent(
      input$dataset,
      input$scenario_type,
      input$previous_scenario
    )

  params_with_inputs <- shiny::reactive({
    p <- params()
    p$dataset <- input$dataset
    p$scenario <- input$scenario
    p$seed <- input$seed
    p$model_runs <- as.numeric(input$model_runs)
    p$start_year <- input$start_year
    p$end_year <- as.numeric(input$end_year)
    p$app_version <- input$app_version

    p
  })

  filename <- shiny::reactive({
    shiny::req(scenario_validation())
    params_filename(session$user, input$dataset, input$scenario)
  })

  # observers ----

  shiny::observe({
    is_power_user <- any(c("nhp_devs", "nhp_power_users") %in% session$groups)
    if (is_local() || is_power_user) {
      shinyjs::enable("app_version")
    }
  })

  # when params change, update inputs
  shiny::observe({
    p <- shiny::req(params())

    y <- p$start_year * 100 + p$start_year %% 100 + 1
    # we don't need to update dataset/cohort:
    # the parameters files that are listed in the previous scenario dropdown are already tied to that provider
    shiny::updateSelectInput(session, "start_year", selected = y)
    shiny::updateSelectInput(session, "end_year", selected = as.character(p$end_year))
    shiny::updateNumericInput(session, "seed", value = p$seed)
    shiny::updateSelectInput(session, "model_runs", selected = p$model_runs)
    shiny::updateSelectInput(session, "app_version", selected = p$app_version)
  }) |>
    shiny::bindEvent(params())

  # update the dataset dropdown when the list of providers changes
  shiny::observe({
    shiny::updateSelectInput(
      session,
      "dataset",
      choices = selected_providers()
    )
  }) |>
    shiny::bindEvent(selected_providers())

  # the end-year range should be 1 year after the start year to the year 2043,
  # which will also be the default.
  shiny::observe({
    x <- as.numeric(stringr::str_sub(input$start_year, 1, 4))

    fy_yyyy <- seq(x + 1, 2041) # 2043 fixed as latest possible end year
    fy_yy <- stringr::str_sub(fy_yyyy + 1, 3, 4)
    fy_choices <- paste(fy_yyyy, fy_yy, sep = "/")
    fy_choices_num <- setNames(fy_yyyy, fy_choices)

    shiny::updateSelectInput(
      session,
      "end_year",
      choices = fy_choices_num,
      selected = max(fy_choices_num)
    )
  }) |>
    shiny::bindEvent(input$start_year)

  # when a user changes the dataset, reset the scenario box back to default (create new from scratch)
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
    shiny::bindEvent(input$dataset)

  # watch the scenario inputs
  # this shows/hides some of the inputs in the scenario box, depending on what is selected in the scenario_type radio
  # buttons
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

  # renders ----
  output$peers_list <- gt::render_gt({
    peers_table(selected_peers())
  })

  output$providers_map <- leaflet::renderLeaflet({
    providers_map(selected_peers())
  })

  output$start_button <- shiny::renderUI({
    if (scenario_validation()) {
      f <- tempfile_name()
      p <- shiny::req(params_with_inputs())
      jsonlite::write_json(p, f, pretty = TRUE, auto_unbox = TRUE)

      # used by the variable in config::get("app_url")
      version <- stringr::str_replace(p$app_version, "\\.", "-") # nolint

      url <- glue::glue(config::get("app_url"), "?", URLencode(basename(f)))

      shiny::tags$a(
        "Start",
        class = "btn btn-success",
        href = url
      )
    }
  }) |>
    shiny::bindEvent(filename(), params_with_inputs())

  # return ----
  NULL
}

shiny::shinyApp(
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$title("NHP: Inputs Selection")
    ),
    shinyjs::useShinyjs(),
    ui
  ),
  server,
  options = list(port = 9080)
)
