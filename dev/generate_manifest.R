files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "app.R",
  fs::dir_ls("R"),
  fs::dir_ls("inst", recurse = TRUE, type = "file")
)

# monkey patch the function to use our profile
assignInNamespace(
  "renvLockFile",
  \(...) "renv/profiles/dev/renv.lock",
  "rsconnect"
)

rsconnect::writeManifest(appFiles = files)
