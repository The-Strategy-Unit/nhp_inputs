shiny::devmode()

selection_app <- callr::r_bg(\() {
  app <- shiny::runApp(
    "inputs_selection_app",
    port = 9080
  )
  print(app)
})

cat("Inputs Selection App: http://127.0.0.1:9080\n")

shiny::runApp()
