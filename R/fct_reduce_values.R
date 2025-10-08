#' Reduce values to ensure sum does not exceed 1
#'
#' Recursively reduces non-target values until the sum of all values is less than
#' or equal to 1. This is useful for ensuring proportions remain valid.
#'
#' @param values A named numeric vector of values between 0 and 1.
#' @param target The name of the target value that should not be reduced.
#'
#' @return A named numeric vector with adjusted values that sum to at most 1.
#' @noRd
reduce_values <- function(values, target) {
  # ensure values are valid
  stopifnot(
    "values must be between 0 and 1" = all(values >= 0, values <= 1)
  )

  # start of including all of the non-target items which are greater than 0
  include <- values[values > 0] |>
    names() |>
    stringr::str_subset(stringr::fixed(target), TRUE)

  # create a recursive function to reduce the values until sum(values) <= 1
  #' Recursive value reduction function
  #'
  #' Internal helper function that recursively reduces non-target values
  #' until their sum is less than or equal to 1.
  #'
  #' @param values Named numeric vector of values to reduce.
  #' @param include Character vector of names of values that can be reduced.
  #'
  #' @return Adjusted numeric vector with sum <= 1.
  #' @noRd
  fn <- function(values, include) {
    # get the sum of the values
    s <- sum(values)
    # if the sum is less than or equal to 1 we are ok
    if (s <= 1) {
      return(values)
    }
    # work out the amount over
    over <- s - 1
    # how many values can we reduce
    n <- length(include)
    # figure out how much to reduce the values by
    # - use either the smallest value in the list,
    # - or equally reduce all of the values
    r <- pmin(min(values[include]), over / n)
    # update the values
    values[include] <- values[include] - r
    # recurse: remove items that are now 0
    fn(values, include[values[include] > 0])
  }

  # run the function
  fn(values, include)
}
