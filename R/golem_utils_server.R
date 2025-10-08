#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
#' Remove NULL elements from a list
#'
#' Filters out NULL elements from a list or vector.
#'
#' @param x A list or vector that may contain NULL elements.
#'
#' @return The input with NULL elements removed.
#' @noRd
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}


# nolint start: object_name_linter

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

# nolint end

#' Create reactive values (shorthand)
#'
#' A convenience wrapper around \code{shiny::reactiveValues()} to reduce typing.
#'
#' @param ... Named values to create reactive variables.
#'
#' @return A reactivevalues object.
#' @noRd
rv <- function(...) shiny::reactiveValues(...)

#' Convert reactive values to list (shorthand)
#'
#' A convenience wrapper around \code{shiny::reactiveValuesToList()} to reduce typing.
#'
#' @param ... Arguments passed to \code{shiny::reactiveValuesToList()}.
#'
#' @return A list containing the reactive values.
#' @noRd
rvtl <- function(...) shiny::reactiveValuesToList(...)
