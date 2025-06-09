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

app_version_choices <- c(
  "v3.6",
  "v3.5",
  "v3.4",
  "v3.3",
  "v3.2",
  "v3.1",
  "v3.0",
  "v2.2",
  "v2.1",
  "v2.0",
  "v1.2",
  "v1.1",
  "v1.0",
  "dev"
)

deploy(
  server = "connect.strategyunitwm.nhs.uk",
  app_id = 215,
  app_version_choices
)

# only use the versions that are deployed to the new server currently
app_version_choices <- c(
  "v3.6",
  "v3.5",
  "v3.4",
  "v3.3",
  "dev"
)

deploy(
  server = "connect.su.mlcsu.org",
  app_id = 71,
  app_version_choices
)
