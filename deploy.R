files <- c(
  dir(".", ".R"),
  dir(".", ".yml"),
  dir(".", "*.json"),
  dir(".", "*.Rds"),
  dir(".", "*.md")
)

files <- files[!files == "deploy.R"]

rsconnect::deployApp(appFiles = files)
