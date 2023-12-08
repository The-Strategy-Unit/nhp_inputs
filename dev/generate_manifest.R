files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "app.R",
  fs::dir_ls("R"),
  fs::dir_ls("inst", recurse = TRUE, type = "file")
)

options(rsconnect.packrat = TRUE)
rsconnect::writeManifest(appFiles = files)
