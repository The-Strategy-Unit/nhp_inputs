app_version_choices <- jsonlite::fromJSON(Sys.getenv(
  "APP_VERSION_CHOICES",
  "[\"dev\"]"
))

# CONSTANTS ----
maximum_model_horizon_year <- 2041
default_baseline_year <- 2023

# HELPERS ----

# until https://github.com/posit-dev/air/issues/256 is resolved, use nolint start/end
# nolint start
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
# nolint end

load_params <- function(file) {
  p <- jsonlite::read_json(file, simplifyVector = TRUE)

  # To trigger UI warnings
  prior_app_version <- p$app_version
  if (is.null(prior_app_version)) prior_app_version <- "new"
  attr(p, "prior_app_version") <- prior_app_version

  # To trigger upgrade methods
  class(p) <- p$app_version
  unclass(upgrade_params(p))
}

upgrade_params <- function(p) {
  UseMethod("upgrade_params", p)
}

upgrade_params.default <- function(p) {
  p
}

upgrade_params.v1.2 <- function(p) {
  p$health_status_adjustment <- TRUE
  class(p) <- p$app_version <- "v2.0"
  upgrade_params(p)
}

upgrade_params.v2.0 <- function(p) {
  class(p) <- p$app_version <- "v2.1"
  upgrade_params(p)
}

upgrade_params.v2.1 <- function(p) {
  class(p) <- p$app_version <- "v2.2"
  upgrade_params(p)
}

upgrade_params.v2.2 <- function(p) {
  class(p) <- p$app_version <- "v3.0"
  upgrade_params(p)
}

upgrade_params.v3.0 <- function(p) {
  class(p) <- p$app_version <- "v3.1"
  upgrade_params(p)
}

upgrade_params.v3.1 <- function(p) {
  class(p) <- p$app_version <- "v3.2"
  upgrade_params(p)
}

upgrade_params.v3.2 <- function(p) {
  # Change NDG params structure

  ndg_values <- p[["non-demographic_adjustment"]]

  # Build new key-value format, assume NDG variant 2
  p[["non-demographic_adjustment"]] <- list(
    "variant" = "variant_2",
    "value-type" = "year-on-year-growth",
    "values" = ndg_values
  )

  # Overwrite variant if variant 1
  is_ndg1 <- identical(ndg_values[["ip"]][["non-elective"]], c(1.0194, 1.0240))
  if (is_ndg1) {
    p[["non-demographic_adjustment"]][["variant"]] <- "variant_1"
  }

  class(p) <- p$app_version <- "v3.3"
  upgrade_params(p)
}

upgrade_params.v3.3 <- function(p) {
  # Remove deprecated AEC mitigators

  aec_mitigators <- paste0(
    "ambulatory_emergency_care_",
    c("low", "moderate", "high", "very_high")
  )

  for (mitigator in aec_mitigators) {
    p[["efficiencies"]][["ip"]][[mitigator]] <- NULL
    p[["time_profile_mappings"]][["efficiencies"]][["ip"]][[mitigator]] <- NULL
    p[["reasons"]][["efficiencies"]][["ip"]][[mitigator]] <- NULL
  }

  # remove the unused demographics file key
  if ("file" %in% names(p[["demographic_factors"]])) {
    p[["demographic_factors"]][["file"]] <- NULL
  }

  class(p) <- p$app_version <- "v3.4"
  upgrade_params(p)
}

upgrade_params.v3.4 <- function(p) {
  # Add (or overwrite) inequalities

  p <- modifyList(
    p,
    list(inequalities = NULL), # model expects "inequalities": {}
    keep.null = TRUE # NULL list elements are usually discarded
  )

  class(p) <- p$app_version <- "v3.5"
  upgrade_params(p)
}

upgrade_params.v3.5 <- function(p) {
  # Set waiting list adjustment to 'off'

  p[["waiting_list_adjustment"]] = list(ip = NULL, op = NULL)

  class(p) <- p$app_version <- "v3.6"
  upgrade_params(p)
}

upgrade_params.v3.6 <- function(p) {
  # Overwrite population growth selections with 'migration category' default
  # variant due to the addition of the new ONS 2022 projections.

  p[["demographic_factors"]][["variant_probabilities"]] <-
    list("migration_category" = 1)

  class(p) <- p$app_version <- "v4.0"
  upgrade_params(p)
}

params_path <- function(user, dataset) {
  path <- file.path(
    config::get("params_data_path"),
    "params",
    user %||% "[development]",
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
  peer_marker <- leaflet::makeAwesomeIcon(
    icon = "medkit",
    library = "fa",
    markerColor = "blue"
  )
  provider_marker <- leaflet::makeAwesomeIcon(
    icon = "medkit",
    library = "fa",
    markerColor = "orange"
  )

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

format_year_as_fyear <- function(year) {
  stopifnot(
    "invalid value for year" = all(year >= 1000 & year <= 9999)
  )

  paste(year, (year + 1) %% 100, sep = "/")
}

generate_year_dropdown_choices <- function(years) {
  fyears <- format_year_as_fyear(years)
  purrr::set_names(as.character(years), fyears)
}

get_version_from_attr <- function(p) {
  prior_app_version <- attr(p, "prior_app_version")

  if (is.null(prior_app_version)) {
    stop("prior_app_version attribute not found on params object p.")
  }

  is_version <- stringr::str_detect(prior_app_version, "^v\\d{1,}\\.\\d{1,}$")
  is_dev_or_new <- stringr::str_detect(prior_app_version, "^(dev|new)$")
  if (!(is_version | is_dev_or_new)) {
    stop("prior_app_version attribute must be in the form 'v1.2' or 'dev'.")
  }

  prior_app_version
}

extract_major_version <- function(version_string) {
  is_dev_or_new <- stringr::str_detect(version_string, "^(dev|new)$")

  if (!is_dev_or_new) {
    version_string <- version_string |>
      stringr::str_remove("v") |>
      as.numeric() |>
      floor()
  }

  version_string
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
      shiny::HTML(markdown::mark_html(
        "home.md",
        output = FALSE,
        template = FALSE
      ))
    )
  )

  # middle column contains the inputs that the user is going to set
  middle_column <- shiny::column(
    width = 4,
    bs4Dash::box(
      title = "Select Provider and Baseline",
      collapsible = FALSE,
      width = 12,
      shiny::selectInput(
        "dataset",
        "Provider",
        choices = NULL,
        selectize = TRUE
      ),
      shiny::selectInput(
        "start_year",
        "Baseline Financial Year",
        # TODO: revisit why start year and end year are formatted differently
        choices = c("2023/24" = 202324),
        selected = as.character(
          (default_baseline_year * 100) + ((default_baseline_year + 1) %% 100)
        )
      ),
      shiny::htmlOutput("baseline_202324_warning"),
      shiny::selectInput(
        "end_year",
        "Model Financial Year",
        choices = generate_year_dropdown_choices(
          (default_baseline_year + 1):maximum_model_horizon_year
        ),
        selected = as.character(maximum_model_horizon_year)
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
        shiny::div(
          id = "pop_proj_warning",
          shiny::HTML(
            "<font color='red'>Your scenario will be upgraded to work with the
            latest version of the model. From v4.0 the model uses the 2022 ONS
            population projections, so your population-growth selections will
            be reset to the new default. Please review this change.</font><p>"
          )
        )
      ),
      shinyjs::hidden(
        shiny::selectInput(
          "previous_scenario",
          "Previous Scenario",
          NULL
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = "start_year_warning",
          shiny::HTML(
            "<font color='red'>The selected scenario has a baseline year prior
            to 2023/24 and cannot be upgraded. See
            <a href='https://connect.strategyunitwm.nhs.uk/nhp/project_information/project_plan_and_summary/model_updates.html#v4.0'>
            the model updates page</a> for details.</font>"
          )
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = "ndg_warning",
          shiny::HTML(
            "<font color='red'>You cannot upgrade a scenario that contains
            Variant 1 of the non-demographic growth (NDG) adjustment. See
            <a href='https://connect.strategyunitwm.nhs.uk/nhp/project_information/project_plan_and_summary/model_updates.html#v3.3'>
            the model updates page</a> for details.</font>"
          )
        )
      ),
      shiny::textInput("scenario", "Name"),
      shiny::div(
        id = "naming_guidance",
        "Please follow",
        shiny::a(
          "the model-run naming guidelines.",
          href = "https://connect.strategyunitwm.nhs.uk/nhp/project_information/user_guide/naming_scenarios.html"
        ),
        style = "margin-top: -5px; margin-bottom: 8px"
      ),
      shiny::uiOutput("start_button")
    ),
    bs4Dash::box(
      title = "Advanced Options",
      width = 12,
      collapsed = TRUE,
      shiny::numericInput("seed", "Seed", sample(1:100000, 1)),
      shiny::selectInput(
        "model_runs",
        "Model Runs",
        choices = c(256, 512, 1024),
        selected = 256
      ),
      shinyjs::disabled(
        shiny::selectInput(
          "app_version",
          "Model Version",
          choices = app_version_choices
        )
      ),
      shinyjs::disabled(
        shinyjs::hidden(
          shiny::selectInput("selected_user", "Selected User", choices = NULL)
        )
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

  providers <- readRDS("providers.Rds")
  all_providers <- jsonlite::read_json(
    "all_providers.json",
    simplifyVector = TRUE
  )

  provider_locations <- sf::read_sf("provider_locations.geojson")

  # reactives ----

  current_user <- shiny::reactive({
    session$user %||% "[development]"
  })

  # each time the user connects we create a temporary file which is what is passed to the main inputs app

  tempfile_name <- shiny::reactive({
    path <- file.path(config::get("params_data_path"), "tmp")
    dir.create(path, FALSE, TRUE)
    tempfile("", tmpdir = path)
  })

  # only show the providers that a user is allowed to access
  selected_providers <- shiny::reactive({
    g <- session$groups

    p <- all_providers

    if (!(is.null(g) || any(c("nhp_devs", "nhp_power_users") %in% g))) {
      a <- g |>
        stringr::str_subset("^nhp_provider_") |>
        stringr::str_remove("^nhp_provider_")
      p <- intersect(p, a)
    }

    p <- providers[providers %in% p]
  })

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
    f <- params_filename(current_user(), input$dataset, input$scenario)

    shiny::validate(
      shiny::need(
        s != "",
        "Scenario name must be completed in order to proceed",
        "Scenario"
      ),
      shiny::need(
        !stringr::str_detect(s, "[^a-zA-Z0-9\\-]"),
        "Scenario can only contain letters, numbers, and - characters",
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
        input$selected_user,
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
    p$user <- current_user()
    # decide whether the results are viewable to all users: if this is false
    # then only nhp_devs/nhp_power_users can view the results
    p$viewable <- any(stringr::str_starts(session$groups, "nhp_provider"))

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
    params_filename(input$selected_user, input$dataset, input$scenario)
  })

  # observers ----

  shiny::observe({
    users <- c(
      dir(
        file.path(
          config::get("params_data_path"),
          "params"
        )
      ),
      current_user()
    ) |>
      unique() |>
      sort()

    shiny::updateSelectInput(
      session,
      "selected_user",
      choices = users,
      selected = current_user()
    )
  })

  shiny::observe({
    is_power_user <- any(c("nhp_devs", "nhp_power_users") %in% session$groups)
    if (is_local() || is_power_user) {
      shinyjs::enable("app_version")
      shinyjs::enable("selected_user")
      shinyjs::show("selected_user")
    }
  })

  # when params change, update inputs
  shiny::observe({
    p <- shiny::req(params())

    stopifnot(
      "start_year is coming through as an fyear, should be yyyy" =
        (p$start_year >= 1000) && (p$start_year <= 9999) # fmt:skip
    )

    if (p$start_year >= 2023) {
      y <- p$start_year * 100 + p$start_year %% 100 + 1
      # we don't need to update dataset: the parameters files that are listed in
      # the previous scenario dropdown are already tied to that provider
      shiny::updateSelectInput(session, "start_year", selected = y)
    }

    selected_end_year <- p$end_year
    if (
      selected_end_year <= p$start_year ||
        selected_end_year > maximum_model_horizon_year
    ) {
      selected_end_year <- maximum_model_horizon_year
    }

    shiny::updateSelectInput(
      session,
      "end_year",
      selected = as.character(p$end_year)
    )
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

  # the end-year range should be 1 year after the start year to the year 2041/42,
  # which will also be the default if starting from scratch.
  shiny::observe({
    start_yr <- as.numeric(stringr::str_sub(input$start_year, 1, 4))

    fy_choices <- generate_year_dropdown_choices(
      (start_yr + 1):maximum_model_horizon_year
    )

    # Set end year to latest year, otherwise the year stored in existing params
    selected_end_year <- if (input$scenario_type == "Create new from scratch") {
      maximum_model_horizon_year
    } else {
      shiny::req(params())$end_year
    }

    shiny::updateSelectInput(
      session,
      "end_year",
      choices = fy_choices,
      selected = selected_end_year
    )
  }) |>
    shiny::bindEvent(input$start_year)

  # when a user changes the dataset, reset the scenario box back to default (create new from scratch)
  shiny::observe({
    ds <- shiny::req(input$dataset)

    saved_params <- params_path(input$selected_user, ds) |>
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
    shiny::bindEvent(input$dataset, input$selected_user)

  # watch the scenario inputs
  # this shows/hides some of the inputs in the scenario box, depending on what is selected in the scenario_type radio
  # buttons
  shiny::observe({
    if (input$selected_user != current_user()) {
      shinyjs::disable("scenario_type")

      shiny::updateCheckboxInput(
        session,
        "scenario_type",
        value = "Create new from existing"
      )
    }

    if (input$scenario_type == "Create new from scratch") {
      shinyjs::show("scenario")
      shinyjs::enable("scenario")
      shinyjs::hide("pop_proj_warning")
      shinyjs::hide("previous_scenario")
      shinyjs::hide("start_year_warning")
      shinyjs::hide("ndg_warning")
      shinyjs::show("naming_guidance")
      shiny::updateTextInput(session, "scenario", value = "")
    } else if (input$scenario_type == "Create new from existing") {
      shinyjs::show("scenario")
      shinyjs::show("previous_scenario")
      shinyjs::show("naming_guidance")
      shiny::updateTextInput(session, "scenario", value = "")
    } else if (input$scenario_type == "Edit existing") {
      shinyjs::hide("scenario")
      shinyjs::show("previous_scenario")
      shinyjs::hide("naming_guidance")
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
    # Toggle element visibility if selecting existing scenarios
    if (stringr::str_detect(input$scenario_type, "existing")) {
      p <- shiny::req(params())

      # Warn about forced upgrade to 2022 pop projections if prior scenario was
      # <v4.0 (ignore warning if dev or new scenario).

      version_string <- get_version_from_attr(p)
      is_dev_or_new <- stringr::str_detect(version_string, "^(dev|new)$")

      shinyjs::toggle(
        "pop_proj_warning",
        condition = !is_dev_or_new && extract_major_version(version_string) < 4
      )

      # Warn user they can't upgrade certain scenarios, disable interaction

      is_deprecated_start_year <- p[["start_year"]] < 2023
      is_ndg1 <- p[["non-demographic_adjustment"]][["variant"]] == "variant_1"

      if (is_deprecated_start_year) {
        shinyjs::show("start_year_warning")
        shinyjs::hide("scenario")
        shinyjs::hide("start_button")
        shinyjs::hide("naming_guidance")
        shinyjs::hide("pop_proj_warning")
      } else if (is_ndg1) {
        shinyjs::show("ndg_warning")
        shinyjs::hide("scenario")
        shinyjs::hide("start_button")
        shinyjs::hide("naming_guidance")
        shinyjs::hide("pop_proj_warning")
      } else {
        shinyjs::hide("start_year_warning")
        shinyjs::hide("ndg_warning")
        shinyjs::enable("scenario")
        shinyjs::show("start_button")
      }
    }

    # Reset element visibility if starting from scratch
    if (input$scenario_type == "Create new from scratch") {
      shinyjs::hide("pop_proj_warning")
      shinyjs::hide("start_year_warning")
      shinyjs::hide("ndg_warning")
      shinyjs::enable("scenario")
      shinyjs::show("start_button")
    }
  }) |>
    shiny::bindEvent(
      input$scenario_type,
      input$previous_scenario
    )

  # 'create new' radio button should force model version dropdown to latest
  shiny::observe({
    if (input$scenario_type == "Create new from scratch") {
      shiny::updateSelectInput(
        session,
        "app_version",
        selected = app_version_choices[1]
      )
    }
  }) |>
    shiny::bindEvent(input$scenario_type)

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

  output$baseline_202324_warning <- shiny::renderText({
    if (input$start_year == "202324") {
      "<font color='red'>You must request and review your 2023/24 detailed
      baseline data before selecting 2023/24 as the baseline year. Please
      contact <a href='mailto:mlcsu.su.datascience@nhs.net?subject=NHP request:
      2023/24 baseline data&body=I am requesting the 2023/24 detailed baseline
      data for [insert scheme name].'>mlcsu.su.datascience@nhs.net</a> to
      request your 2023/24 detailed baseline data.</font><br><br>"
    }
  }) |>
    shiny::bindEvent(input$start_year)

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
  server
)
