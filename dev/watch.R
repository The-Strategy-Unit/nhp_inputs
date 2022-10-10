watch_files_and_start_task <- function(task_fn, ..., delay_time = 1, max_retries = 3) {
  files <- c(...)
  stopifnot("... argument not all characters" = is.character(files))
  get_modified_time <- \() max(fs::file_info(files)$modification_time)
  # start the task and get the current file modified time
  task <- callr::r_bg(task_fn)
  previous_max_time <- get_modified_time()
  # main loop
  retry_count <- 0 # in case tasks fail
  repeat {
    # show any output from the task
    while ((output <- task$read_output()) != "") {
      cat(output)
    }
    # show any errors from the task
    while ((error <- task$read_error()) != "") {
      cat(error)
    }
    # check the task is alive, if it isn't increment our retry counter
    if (!task$is_alive()) {
      # if we try to retry the task too many times, exit
      stopifnot("task has failed to start" = retry_count == max_retries)
      retry_count <- retry_count + 1
      previous_max_time <- 0
    } else {
      # reset the retry counter as the task is running
      retry_count <- 0
    }

    # see if any of the files have changed since the last loop iteration
    new_max_time <- max(fs::file_info(files)$modification_time)
    # if files have changed, restart the task
    if (new_max_time > previous_max_time) {
      cli::cli_alert_info("{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}: restarting app")
      task$kill()
      task <- callr::r_bg(task_fn)

      previous_max_time <- new_max_time
    }

    # have a slight delay before checking again
    Sys.sleep(delay_time)
  }
}

watch_files_and_start_task(
  \() print(golem::run_dev()),
  fs::dir_ls(path = c("R"), recurse = TRUE, glob = "*.R"),
  "inst/golem-config.yml",
  "DESCRIPTION"
)
