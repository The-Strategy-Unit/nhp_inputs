deploy <- function(server, app_id, app_version_choices) {
  if (!file.exists("deploy.R") && dir.exists("inputs_selection_app")) {
    withr::local_dir("inputs_selection_app")
  }
  stopifnot(
    "Need to run inside the inputs_selection_app folder" = file.exists(
      "deploy.R"
    )
  )

  files <- c(
    dir(".", ".R"),
    dir(".", ".yml"),
    dir(".", "*.json"),
    dir(".", "*.Rds"),
    dir(".", "*.md")
  )

  files <- files[!files == "deploy.R"]

  withr::local_envvar(
    APP_VERSION_CHOICES = jsonlite::toJSON(
      app_version_choices,
      auto_unbox = TRUE
    ),
    R_CONFIG_ACTIVE = "production"
  )

  rsconnect::deployApp(
    appId = app_id,
    server = server,
    appFiles = files,
    appName = "nhp-inputs_selection",
    appTitle = "NHP: Inputs Selection",
    envVars = c(
      "APP_VERSION_CHOICES",
      "R_CONFIG_ACTIVE"
    )
  )
}

# only use the versions that are deployed to the new server currently
app_version_choices <- c(
  "v4.0",
  "v3.6",
  "v3.5",
  "v3.4",
  "v3.3",
  "dev"
)

deploy(
  server = "connect.strategyunitwm.nhs.uk",
  app_id = 71,
  app_version_choices
)
