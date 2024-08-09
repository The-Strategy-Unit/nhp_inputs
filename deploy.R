deploy <- function(...) {
  stopifnot("Need to run inside the inputs_selection_app folder" = file.exists("deploy.R"))

  app_version_choices <- list(...)

  files <- c(
    dir(".", ".R"),
    dir(".", ".yml"),
    dir(".", "*.json"),
    dir(".", "*.Rds"),
    dir(".", "*.md")
  )

  files <- files[!files == "deploy.R"]

  withr::local_envvar(
    APP_VERSION_CHOICES = jsonlite::toJSON(app_version_choices, auto_unbox = TRUE)
  )

  rsconnect::deployApp(
    appId = 215,
    appFiles = files,
    envVars = c(
      "APP_VERSION_CHOICES"
    )
  )
}

deploy("v2.1", "v2.0", "v1.2", "v1.1", "v1.0", "dev")
