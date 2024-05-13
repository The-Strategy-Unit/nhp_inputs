files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "app.R",
  fs::dir_ls("R"),
  fs::dir_ls("inst", recurse = TRUE, type = "file")
)

rsconnect::writeManifest(appFiles = files)

system("node remove_files_from_manifest.js")
