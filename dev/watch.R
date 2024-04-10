withr::with_dir(
  "inputs_selection_app",
  {
    fn <- \() {
      source("app.R")
      print(app)
    }

    results_selection_app <- callr::r_bg(fn)
  }
)
cat("Inputs Selection App: http://127.0.0.1:9080\n")

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
