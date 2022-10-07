get_max_modification_time <- function(...) {
  files <- fs::dir_ls(recurse = TRUE, ...)

  max(fs::file_info(files)$modification_time)
}

# watch_files
previous_max_time <- 0
task <- list(kill = \() TRUE)
while (TRUE) {
  new_max_time <- get_max_modification_time(path = c("R"), glob = "*.R")

  if (new_max_time > previous_max_time) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": restarting app\n", sep = "")
    task$kill()
    task <- callr::r_bg(\() source("dev/run_dev.R", echo = TRUE))
    previous_max_time <- new_max_time
  }

  Sys.sleep(1)
}
