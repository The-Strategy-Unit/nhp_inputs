files <- c(
  dir(".", ".R"),
  dir(".", ".yml"),
  dir(".", "*.json"),
  dir(".", "*.Rds"),
  dir(".", "*.md")
)

files <- files[!files == "deploy.R"]

rsconnect::deployApp(appId = 215, appFiles = files)
