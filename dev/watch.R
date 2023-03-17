watch_files_and_start_task <- function(task_fn, ..., delay_time = 1, max_retries = 3) {
  files <- c(...)
  stopifnot("... argument not all characters" = is.character(files))
  get_modified_time <- \() max(fs::file_info(files)$modification_time)

  task <- list(kill = \() NULL)
  previous_max_time <- -Inf
  retry_count <- 0 # in case tasks fail

  repeat {
    # see if any of the files have changed since the last loop iteration
    new_max_time <- max(fs::file_info(files)$modification_time)
    # if files have changed, restart the task
    if (new_max_time > previous_max_time) {
      cli::cli_alert_info("{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}: restarting app")
      task$kill()
      task <- callr::r_bg(task_fn)

      previous_max_time <- new_max_time
    }

    # show any output from the task
    while ((output <- task$read_output()) != "") {
      cat(output)
    }
    # show any errors from the task
    while ((error <- task$read_error()) != "") {
      cat(error)
    }

    # have a slight delay before checking again
    Sys.sleep(delay_time)
  }
}

watch_files_and_start_task(
  \() {
    try({
      app <- golem::run_dev()
      print(app)
    })
  },
  fs::dir_ls(path = c("R"), recurse = TRUE, glob = "*.R"),
  "inst/golem-config.yml",
  "DESCRIPTION"
)
