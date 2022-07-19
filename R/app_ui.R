#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  header <- bs4Dash::dashboardHeader(title = "NHP Model Inputs")

  sidebar <- bs4Dash::dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    bs4Dash::sidebarMenu(
      id = "sidebarMenu",
      bs4Dash::menuItem(
        "Home",
        tabName = "tab_home",
        icon = shiny::icon("house")
      )
    )
  )

  body <- bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "tab_home",
        shiny::fluidRow(
          col_6(
            htmltools::h1("NHP Model Inputs")
            # "About Text should go here"
          )
        )
      )
    )
  )

  shiny::tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    bs4Dash::dashboardPage(
      header,
      sidebar,
      body
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "nhp_inputs"
    )
  )
}
