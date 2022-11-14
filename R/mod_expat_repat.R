#' expat_repat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expat_repat_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$h1("Expatriation/Repatriation"),
    shiny::fluidRow(
      bs4Dash::box(
        title = "Selection",
        width = 2,
        shiny::selectInput(
          ns("activity_type"),
          "Activity Type",
          c(
            "Inpatients" = "ip",
            "Outpatients" = "op",
            "A&E" = "aae"
          )
        ),
        shinyjs::hidden(
          shiny::selectInput(
            ns("ip_subgroup"),
            "Subgroup",
            c(
              "Elective" = "elective",
              "Non-Elective" = "non-elective",
              "Maternity" = "maternity"
            )
          )
        ),
        shiny::selectInput(
          ns("type"),
          NULL,
          NULL
        )
      ),
      bs4Dash::box(
        title = "Model Parameter",
        width = 10,
        purrr::pmap(
          list(
            title = c("Expat", "Repat (Local)", "Repat (Non-Local)"),
            type = c("expat", "repat_local", "repat_nonlocal"),
            min = c(0, 100, 100),
            max = c(100, 200, 200),
            values = list(c(95, 100), c(100, 105), c(100, 105))
          ),
          \(title, type, min, max, values) {
            shiny::tagList(
              shiny::tags$h3(title),
              shiny::checkboxInput(
                ns(glue::glue("include_{type}")),
                label = "Include?"
              ),
              shinyjs::disabled(
                shiny::sliderInput(
                  ns(type),
                  "Confidence Interval",
                  min, max, values, 0.1,
                  post = "%"
                )
              ),
              shiny::plotOutput(ns(glue::glue("{type}_plot")))
            )
          }
        ) |>
          shiny::tagList()
      )
    )
  )
}

#' expat_repat Server Functions
#'
#' @noRd
mod_expat_repat_server <- function(id, params, provider, baseline_year) {
  shiny::moduleServer(id, function(input, output, session) {
    rtt_specialties <- readRDS(app_sys("app", "data", "rtt_specialties.Rds"))
    expat_repat_data <- readRDS(app_sys("app", "data", "expat_repat_data.Rds"))

    # helper method to construct the initial values for our params
    init_params <- function(values) {
      if (missing(values)) {
        rtt_specs <- list()
        aae_groups <- list()
      } else {
        rtt_specs <- purrr::map(purrr::set_names(rtt_specialties), ~values)
        aae_groups <- list(
          "ambulance" = values,
          "walk-in" = values
        )
      }

      list(
        "ip" = purrr::map(
          purrr::set_names(c("elective", "non-elective", "maternity")),
          ~rtt_specs,
        ),
        "op" = rtt_specs,
        "aae" = aae_groups
      )
    }

    # two reactiveValues to keep track of the slider values
    # shadow_params always stores a value for each item selectable by the dropdowns
    # params contains the returned values, and will contain the value from shadow_params if "include" is checked
    shadow_params <- purrr::lift_dl(shiny::reactiveValues)(
      list(
        "expat" = init_params(c(0.95, 1.0)),
        "repat_local" = init_params(c(1.0, 1.05)),
        "repat_nonlocal" = init_params(c(1.0, 1.05))
      )
    )
    params$expat <- init_params()
    params$repat_local <- init_params()
    params$repat_nonlocal <- init_params()

    # update the options in the type drop down based on the activity type dropdown
    # also, toggle whether the ip_subgroup is visible or not
    shiny::observe({
      at <- shiny::req(input$activity_type)

      shinyjs::toggle("ip_subgroup", condition = at == "ip")

      if (at == "aae") {
        type_label <- "Attendance Type"
        type_values <- c(
          "Ambulance" = "ambulance",
          "Walk-In" = "walk-in"
        )
      } else {
        type_label <- "Specialty"
        type_values <- rtt_specialties
      }
      shiny::updateSelectInput(session, "type", type_label, type_values)
    }) |>
      shiny::bindEvent(input$activity_type)

    # watch for changes to the dropdowns
    # update the sliders to the values for the combination of the drop downs in shadow_params
    # set the include checkboxes value if a value exists in params or not
    shiny::observe({
      purrr::walk(
        c("expat", "repat_local", "repat_nonlocal"),
        \(type) {
          at <- shiny::req(input$activity_type)
          st <- shiny::req(input$ip_subgroup)
          t <- shiny::req(input$type)

          sp <- shadow_params[[type]][[at]]
          p <- params[[type]][[at]]
          if (at == "ip") {
            sp <- sp[[st]]
            p <- p[[st]]
          }
          sp <- sp[[t]]
          p <- p[[t]]

          shiny::updateCheckboxInput(session, glue::glue("include_{type}"), value = !is.null(p))
          shiny::updateSliderInput(session, type, value = sp * 100)
        }
      )
    }) |>
      shiny::bindEvent(input$activity_type, input$ip_subgroup, input$type)

    purrr::walk(
      c("expat", "repat_local", "repat_nonlocal"),
      \(type) {
        include_type <- glue::glue("include_{type}")

        # watch the slider values and the include check boxes
        # if the slider value changes then we update the value of the shadow_params to the new slider values
        # set the params to be the slider values if include is checked
        # if it is checked, set the value to null (i.e. delete it from the list)
        shiny::observe({
          at <- shiny::req(input$activity_type)
          st <- shiny::req(input$ip_subgroup)
          t <- shiny::req(input$type)

          include <- input[[include_type]]
          v <- shiny::req(input[[type]]) / 100

          if (at == "ip") {
            shadow_params[[type]][[at]][[st]][[t]] <- v

            params[[type]][[at]][[st]][[t]] <- if (include) v
          } else {
            shadow_params[[type]][[at]][[t]] <- v
            params[[type]][[at]][[t]] <- if (include) v
          }
        }) |>
          shiny::bindEvent(input[[type]], input[[include_type]])

        shiny::observe({
          shinyjs::toggleState(type, condition = input[[include_type]])
        }) |>
          shiny::bindEvent(input[[include_type]])
      }
    )

    # data for charts
    repat_local <- shiny::reactive({
      at <- shiny::req(input$activity_type)
      st <- shiny::req(input$ip_subgroup)
      t <- shiny::req(input$type)

      expat_repat_data$repat_local[[at]] |>
        dplyr::filter(
          .data$procode == provider(),
          if (at == "ip") .data$admigroup == st else TRUE,
          if (at == "aae") .data$is_ambulance == (t == "ambulance") else .data$specialty == t
        ) |>
        dplyr::select(
          .data$fyear,
          .data$provider_n,
          .data$icb_n,
          .data$pcnt
        )
    })

    output$repat_local_plot <- shiny::renderPlot({
      df <- repat_local()
      shiny::req(nrow(df) > 0)

      v <- dplyr::filter(df, .data$fyear == baseline_year())$pcnt

      include <- input$include_repat_local
      values <- input$repat_local * v / 100

      interval <- if (include) {
        ggplot2::annotate(
          "rect",
          xmin = -Inf,
          xmax = Inf,
          ymin = values[[1]],
          ymax = values[[2]],
          colour = "#f9bf07",
          fill = ggplot2::alpha("#f9bf07", 0.2),
          na.rm = TRUE
        )
      }

      df |>
        ggplot2::ggplot(ggplot2::aes(as.factor(.data$fyear), .data$pcnt, group = 1)) +
        interval +
        ggplot2::geom_line() +
        ggplot2::geom_point(
          data = \(.x) dplyr::filter(.x, .data$fyear == baseline_year()),
          colour = "red"
        ) +
        ggplot2::scale_x_discrete(
          labels = \(.x) stringr::str_replace(.x, "^(\\d{4})(\\d{2})$", "\\1/\\2")
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent,
          limits = c(floor(min(df$pcnt * 10)), ceiling(v * 20)) / 10
        ) +
        ggplot2::labs(
          x = "Financial Year",
          y = "Percentange of ICB's activity Delivered by this Provider"
        ) +
        ggplot2::theme(
          legend.position = "none",
          panel.background = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line("#9d928a", linetype = "dotted")
        )
    })
  })
}
