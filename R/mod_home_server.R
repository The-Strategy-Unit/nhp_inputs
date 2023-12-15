#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, providers) {
  shiny::moduleServer(id, function(input, output, session) {
    # static data ----
    peers <- load_rds_from_adls("peers.rds")
    nhp_current_cohort <- load_rds_from_adls("nhp_current_cohort.rds")

    all_providers <- jsonlite::read_json(app_sys("app", "data", "all_providers.json"), simplifyVector = TRUE)

    provider_locations <- sf::read_sf(app_sys("app", "data", "provider_locations.geojson"))

    # reactives ----

    # set up the applications values store, all of the parameters for the model will be stored in this reactiveValues
    params <- shiny::reactiveValues(
      "user" = session$user %||% "[development]",
      "app_version" = Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev"),
      "interval_type" = 0.8
    )

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

    # observers ----

    # update the dataset dropdown when the list of providers changes
    shiny::observe({
      shiny::updateSelectInput(
        session,
        "dataset",
        choices = selected_providers()
      )
    }) |>
      shiny::bindEvent(selected_providers())

    # when the user changes the year, update the end year slider. the end year should be 1 year after the start year
    # to 20 years after, and it will default to 15 years after the start year
    shiny::observe({
      x <- as.numeric(stringr::str_sub(input$start_year, 1, 4))

      fy_yyyy <- seq(x + 1, x + 20)
      fy_yy <- stringr::str_sub(fy_yyyy + 1, 3, 4)
      fy_choices <- paste(fy_yyyy, fy_yy, sep = "/")

      shiny::updateSelectInput(
        session,
        "end_year",
        choices = setNames(x + (0:19), fy_choices),
        selected = x + 15
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

    # watch for uploaded files, and replace the values in params with the contents of the file
    shiny::observe({
      # TODO: should we remove file uploads?
      # this code needs to carefully replicate the next observer
      p <- load_params(input$param_upload$datapath)

      if (p$dataset == "synthetic") {
        cat("skipping changing the dataset\n")
      } else {
        shiny::updateSelectInput(session, "dataset", selected = p$dataset)
      }

      shiny::updateTextInput(session, "scenario", value = p$scenario)

      y <- p$start_year * 100 + p$start_year %% 100 + 1
      shiny::updateSelectInput(session, "start_year", selected = y)
      shiny::updateSelectInput(session, "end_year", selected = as.character(p$end_year))
      shiny::updateNumericInput(session, "seed", value = p$seed)
      shiny::updateSelectInput(session, "model_runs", selected = p$model_runs)

      # copy the loaded params into params
      for (i in names(p)) {
        params[[i]] <- p[[i]]
      }
    }) |>
      shiny::bindEvent(input$param_upload)

    # load the selected params
    # if the user chooses to create new from scratch, we use the default parameters file
    # otherwise, load the values for the scenario the user selected
    shiny::observe({
      default_params <- app_sys("app", "default_params.json")
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

      # if we use the default parameters, don't try to alter these inputs (they don't exist in the default params)
      if (file != default_params) {
        y <- p$start_year * 100 + p$start_year %% 100 + 1
        shiny::updateSelectInput(session, "start_year", selected = y)
        shiny::updateSelectInput(session, "end_year", selected = as.character(p$end_year))
        shiny::updateNumericInput(session, "seed", value = p$seed)
        shiny::updateSelectInput(session, "model_runs", selected = p$model_runs)
      }

      # copy the loaded params into params
      for (i in names(p)) {
        params[[i]] <- p[[i]]
      }
    }) |>
      shiny::bindEvent(
        input$dataset,
        input$scenario_type,
        input$previous_scenario
      )

    # watch the scenario validation - if valid then hide the state and enable the start button
    shiny::observe({
      x <- tryCatch(scenario_validation(), error = \(...) FALSE)
      shinyjs::toggle("status", condition = !x)
      shinyjs::toggleState("start", condition = x)

      # change the colour of the button from grey to green
      shinyjs::toggleClass("start", "btn-default", !x)
      shinyjs::toggleClass("start", "btn-success", x)
    })

    # update the params items with the selections
    shiny::observe({
      params$dataset <- input$dataset
      params$scenario <- input$scenario
      params$seed <- input$seed
      params$model_runs <- as.numeric(input$model_runs)
      params$start_year <- input$start_year
      params$end_year <- as.numeric(input$end_year)
    })

    # renders ----
    output$peers_list <- gt::render_gt({
      mod_home_peers_table(selected_peers())
    })

    output$providers_map <- leaflet::renderLeaflet({
      mod_home_providers_map(selected_peers())
    })

    # the status text is used only for showing the validation of scenario - you need to bind validate to an output
    # in cases where the scenario has been validated, the value will be TRUE, but we hide the output in this case
    output$status <- shiny::renderText(scenario_validation())

    # return ----
    # the start input so we can observe it in the main server
    list(params, shiny::reactive(input$start))
  })
}
