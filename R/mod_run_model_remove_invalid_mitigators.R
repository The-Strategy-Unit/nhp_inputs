mod_run_model_remove_invalid_mitigators <- function(p, schema) {
  json_p <- jsonlite::toJSON(p, auto_unbox = TRUE)

  paths <- schema$validate(json_p, verbose = TRUE) |>
    attr("errors") |>
    dplyr::filter(
      .data[["keyword"]] == "additionalProperties",
      .data[["dataPath"]] |>
        stringr::str_detect("/(activity_avoidance|efficiencies)/")
    ) |>
    dplyr::select("params", "dataPath") |>
    tidyr::unnest_wider("params") |>
    purrr::pmap(
      # nolint start
      # these are the names of the columns in the created dataframe, so ignore lint errors
      \(dataPath, additionalProperty, ...) {
        # nolint end
        path <- stringr::str_split(dataPath, "/")[[1]][-1]
        c(path, additionalProperty)
      }
    )

  remove_item <- function(x, path) {
    if (length(path) < 1) {
      return(x)
    }
    if (length(path) == 1) {
      x[[path]] <- NULL
      return(x)
    }

    p1 <- path[[1]]
    ps <- path[-1]
    sublist <- x[[p1]]
    if (!is.list(sublist)) {
      return(x)
    }

    x[[p1]] <- remove_item(sublist, ps)

    x
  }

  for (i in paths) {
    p <- remove_item(p, i)
  }

  p
}
