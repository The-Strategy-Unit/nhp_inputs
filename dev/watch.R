shiny::devmode()

selection_app <- callr::r_bg(\() {
  app <- nhp.inputs.selection.app::run_app(port = 9080)
  print(app)
})

cat("Inputs Selection App: http://127.0.0.1:9080\n")

shiny::runApp()
