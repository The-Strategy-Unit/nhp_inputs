# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

# print and validate environment variables
envvars_valid <- {
  cat("listing environment variables:\n")

  name_padding_size <- envvars |>
    names() |>
    stringr::str_length() |>
    max()

  envvar_error <- FALSE
  for (i in names(envvars)) {
    v <- envvars[[i]]
    if (i != "CACHE_VERSION") {
      envvar_error <- envvar_error || v == ""
    }

    cat(
      "  * ",
      stringr::str_pad(i, name_padding_size, side = "right"),
      " : ",
      dplyr::case_when(
        v == "" ~ "(not set)",
        stringr::str_detect(i, "KEY") ~ "***",
        .default = v
      ),
      "\n",
      sep = ""
    )
  }

  if (envvar_error) {
    stop(
      "One or more required environment variables are not set. ",
      "Please check the output above for details."
    )
  }
}

run_app() # add parameters here (if any)
