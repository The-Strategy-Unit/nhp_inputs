watchr::watch_files_and_start_task(
  \() {
    try({
      pkgload::load_all()
      print(run_app())
    })
  },
  \() fs::dir_ls(path = c("R"), recurse = TRUE, glob = "*.R"),
  "inst/golem-config.yml",
  "DESCRIPTION"
)
