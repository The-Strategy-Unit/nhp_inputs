selection_app <- callr::r_bg(\() {
  app <- shiny::runApp(
    "inputs_selection_app",
    port = 9080
  )
  print(app)
})

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
