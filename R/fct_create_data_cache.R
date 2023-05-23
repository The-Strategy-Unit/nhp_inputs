#' create_data_cache
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

create_data_cache <- function() {
  if (!dir.exists(".cache")) {
    dir.create(".cache")
  }

  dc <- cachem::cache_disk(".cache/data_cache", 200 * 1024^2) # 200MB

  # in case we need to invalidate the cache on rsconnect quickly, we can increment the "CACHE_VERSION" env var
  cache_version <- ifelse(
    file.exists(".cache/cache_version.txt"),
    as.numeric(readLines(".cache/cache_version.txt")),
    -1
  )

  if (Sys.getenv("CACHE_VERSION", 0) > cache_version) {
    cat("Invalidating cache\n")
    dc$reset()
    cache_version <- Sys.getenv("CACHE_VERSION", 0)
    writeLines(as.character(cache_version), ".cache/cache_version.txt")
  }

  shiny::shinyOptions(cache = dc)

  invisible(NULL)
}
