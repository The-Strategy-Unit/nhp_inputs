#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @examples
#' list_to_li(c("a", "b"))
#' @importFrom shiny tags tagAppendAttributes tagList
#' Convert list to HTML list items
#'
#' Transforms a list into HTML \code{<li>} elements.
#'
#' @param list A list of items to convert.
#' @param class Optional CSS class to apply to the list items.
#'
#' @return A list of HTML \code{<li>} tags.
#' @noRd
list_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$li
      )
    )
  } else {
    res <- lapply(
      list,
      tags$li
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}
#' Turn an R list into corresponding HTML paragraph tags
#'
#' @param list an R list
#' @param class a class for the paragraph tags
#'
#' @return An HTML tag
#' @noRd
#'
#' @examples
#' list_to_p(c("This is the first paragraph", "this is the second paragraph"))
#' @importFrom shiny tags tagAppendAttributes tagList
#'
#' Convert list to HTML paragraphs
#'
#' Transforms a list into HTML \code{<p>} elements.
#'
#' @param list A list of items to convert.
#' @param class Optional CSS class to apply to the paragraphs.
#'
#' @return A list of HTML \code{<p>} tags.
#' @noRd
list_to_p <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$p
      )
    )
  } else {
    res <- lapply(
      list,
      tags$p
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' @importFrom shiny tags tagAppendAttributes tagList
#' Convert named list to HTML list items
#'
#' Transforms a named list into HTML \code{<li>} elements where names are bold
#' and values are regular text.
#'
#' @param list A named list to convert.
#' @param class Optional CSS class to apply to the list items.
#'
#' @return A list of HTML \code{<li>} tags with formatted name-value pairs.
#' @noRd
named_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove attributes from an HTML tag
#'
#' Removes specified attributes from an HTML tag object.
#'
#' @param tag An HTML tag object.
#' @param ... Names of attributes to remove.
#'
#' @return The tag with specified attributes removed.
#' @noRd
#'
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tag_remove_attributes(a, "src")
tag_remove_attributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[attrs[i]]] <- NULL
  }
  tag
}

#' Hide an HTML tag
#'
#' Sets the display CSS property to 'none' to hide a tag.
#'
#' @param tag An HTML tag object to hide.
#'
#' @return The tag with display: none style applied.
#' @noRd
#'
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' @importFrom shiny tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) &&
      !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;",
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' Display a hidden HTML tag
#'
#' Removes the display: none CSS property from a tag to make it visible.
#'
#' @param tag An HTML tag object to display.
#'
#' @return The tag with display: none removed from its style.
#' @noRd
#' @importFrom shiny tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) &&
      grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*",
      "",
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#'
#' @param id the id of the element to hide
#'
#' @noRd
#'
#' @importFrom shiny tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#'
#' @examples
#' with_red_star("Enter your name here")
#' @importFrom shiny tags HTML
with_red_star <- function(text) {
  shiny::tags$span(
    HTML(
      paste0(
        text,
        shiny::tags$span(
          style = "color:red",
          "*"
        )
      )
    )
  )
}


#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#'
#' @examples
#' rep_br(5)
#' @importFrom shiny HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' @importFrom shiny tags
enurl <- function(url, text) {
  tags$a(href = url, text)
}

#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...) {
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...) {
  column(10, ...)
}

#' @importFrom shiny column
col_9 <- function(...) {
  column(9, ...)
}

#' @importFrom shiny column
col_8 <- function(...) {
  column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...) {
  column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...) {
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...) {
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...) {
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...) {
  column(1, ...)
}


#' Make the current tag behave like an action button
#'
#' Only works with compatible tags like button or links
#'
#' @param tag Any compatible tag.
#' @param input_id Unique id. This will host the input value to be used
#' on the server side.
#'
#' @return The modified tag with an extra id and the action button class.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   link <- a(href = "#", "My super link", style = "color: lightblue;")
#'
#'   ui <- fluidPage(
#'     make_action_button(link, input_id = "mylink")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$mylink, {
#'       showNotification("Pouic!")
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
make_action_button <- function(tag, input_id = NULL) {
  # some obvious checks
  if (!inherits(tag, "shiny.tag")) {
    stop("Must provide a shiny tag.")
  }
  if (!is.null(tag$attribs$class)) {
    if (grep("action-button", tag$attribs$class)) {
      stop("tag is already an action button")
    }
  }
  if (is.null(input_id) && is.null(tag$attribs$id)) {
    stop(
      "tag does not have any id. Please use input_id to be able to
           access it on the server side."
    )
  }

  # handle id
  if (!is.null(input_id)) {
    if (!is.null(tag$attribs$id)) {
      warning(
        paste(
          "tag already has an id. Please use input$",
          tag$attribs$id,
          "to access it from the server side. input_id will be ignored."
        )
      )
    } else {
      tag$attribs$id <- input_id
    }
  }

  # handle class
  if (is.null(tag$attribs$class)) {
    tag$attribs$class <- "action-button"
  } else {
    tag$attribs$class <- paste(tag$attribs$class, "action-button")
  }
  # return tag
  tag
}
