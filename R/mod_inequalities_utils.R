#' Initialise inequality choice table
#'
#' Creates a tibble containing unique HRG codes for selected provider and
#' a "choice" column indicating the type of adjustment to apply.
#' Set to "No change" by default
#'
#' If editing an existing scenario, previous parameters will be loaded.
#'
#' @param provider_inequalities A data frame containing provider inequality data.
#' @param params The standard params object
#'
#' @return A tibble with two columns:
#'   \item{hrg_code}{Character vector of unique HRG codes from the input data}
#'   \item{choice}{Character vector indicating the inequality adjustment choice
#'     for each HRG code.}
#'     Possible values are defined in get_inequality_choice_mappings()
#'

initialise_hrg_table <- function(provider_inequalities, params) {
  # Get unique HRG codes from the data
  unique_hrg_codes <- unique(provider_inequalities$sushrg_trimmed)

  hrg_table <- tibble::tibble(
    hrg_code = unique_hrg_codes,
    choice = "No change"
  )

  # Check if params$inequalities has existing data
  if (!is.null(params$inequalities) && length(params$inequalities) > 0) {
    # Update choices based on existing params$inequalities
    for (choice_type in names(params$inequalities)) {
      display_choice <- inequality_choices_to_display(choice_type)

      # Get the HRG codes for this choice type
      hrg_codes_for_choice <- params$inequalities[[choice_type]]

      # Update the selections for matching HRG codes
      hrg_table$choice[
        hrg_table$hrg_code %in% hrg_codes_for_choice
      ] <- display_choice
    }
  }

  hrg_table
}

#' Get Inequality Choice Mappings
#
#' Returns the mapping between programmatic snake_case and display
#' names for inequality adjustment choices.
#'
#' @return Named character vector where names are snake_case identifiers and
#'   values are display names.
get_inequality_choice_mappings <- function() {
  c(
    no_change = "No change",
    zero_sum = "Zero-sum",
    level_up = "Level up",
    level_down = "Level down"
  )
}

#' Convert inequality choices to snake case
#'
#' @param display_choice Character vector of display names e.g., Zero-sum"
#'
#' @return Character vector of snake_case names. Returns \code{NA} for
#'   unrecognized values.
#'
#' @examples
#' inequality_choices_to_snake("Zero-sum")
#'
inequality_choices_to_snake <- function(display_choice) {
  mappings <- get_inequality_choice_mappings()
  names(mappings)[match(display_choice, mappings)]
}

#' Convert inequality choices to display case
#'
#' @param snake_choice Character vector of snake_case names e.g., 'zero_sum'
#'
#' @return Character vector of display names. Returns \code{NA} for
#'   unrecognized values.
#'
#' @examples
#' inequality_choices_to_display("zero_sum")
#'
inequality_choices_to_display <- function(snake_choice) {
  mappings <- get_inequality_choice_mappings()
  unname(mappings[snake_choice])
}
