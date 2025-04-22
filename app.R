# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

cat(
  "listing environment variables:\n",
  "  * AZ_STORAGE_CONTAINER:   ",
  Sys.getenv("AZ_STORAGE_CONTAINER"),
  "\n",
  "  * AZ_STORAGE_EP:          ",
  Sys.getenv("AZ_STORAGE_EP"),
  "\n",
  "  * NHP_API_URI:            ",
  Sys.getenv("NHP_API_URI"),
  "\n",
  "  * NHP_INPUTS_DATA_VERSION:",
  Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev"),
  "\n",
  "  * NHP_CONTAINER_VERSION:  ",
  Sys.getenv("NHP_CONTAINER_VERSION", "dev"),
  "\n",
  "  * NHP_OUTPUTS_URI:        ",
  Sys.getenv("NHP_OUTPUTS_URI"),
  "\n",
  "  * CACHE_VERSION:          ",
  Sys.getenv("CACHE_VERSION"),
  "\n"
)

inputs::run_app() # add parameters here (if any)
