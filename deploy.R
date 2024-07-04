files <- c(
  dir(".", ".R"),
  dir(".", ".yml"),
  dir(".", "*.json"),
  dir(".", "*.Rds"),
  dir(".", "*.md")
)

files <- files[!files == "deploy.R"]

Sys.setenv(
  APP_VERSION_CHOICES = jsonlite::toJSON(
    c("v2.0", "v1.2", "v1.1", "v1.0", "dev"),
    auto_unbox = TRUE
  )
)

rsconnect::deployApp(
  appId = 215,
  appFiles = files,
  envVars = c(
    "APP_VERSION_CHOICES"
  )
)
