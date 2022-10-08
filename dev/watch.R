get_max_modification_time <- function(files) {
  max(fs::file_info(files)$modification_time)
}

# watch_files
previous_max_time <- 0
task <- list(kill = \() TRUE)
while (TRUE) {
  new_max_time <- c(
    fs::dir_ls(path = c("R"), recurse = TRUE, glob = "*.R"),
    "inst/golem-config.yml",
    "DESCRIPTION"
  ) |>
    get_max_modification_time()

  if (new_max_time > previous_max_time) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": restarting app\n", sep = "")
    task$kill()
    task <- callr::r_bg(\() source("dev/run_dev.R", echo = TRUE))
    previous_max_time <- new_max_time
  }

  Sys.sleep(1)
}
