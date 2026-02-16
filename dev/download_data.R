get_data_files <- function(
  inputs_data_version = Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev")
) {
  data_path <- file.path("inst", "app", "data")
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }

  files <- c(
    "rates",
    "age_sex",
    "diagnoses",
    "procedures",
    "baseline",
    "inequalities",
    "expat",
    "repat_local",
    "repat_nonlocal"
  )

  purrr::walk(
    purrr::set_names(files),
    download_provider_data,
    inputs_data_version = inputs_data_version,
    data_path = data_path,
    overwrite = TRUE
  )

  download_params_schema(
    data_path = data_path,
    app_version = inputs_data_version
  )

  invisible(NULL)
}

get_data_files()
