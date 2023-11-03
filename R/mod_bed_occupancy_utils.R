mod_bed_occupancy_specialties <- function() {
  readr::read_csv(
    app_sys("app/data/bed_occupancy_specialties.csv"),
    col_types = "ccc"
  )
}

mod_bed_occupancy_specialty_table <- function(specialties, ns = identity) {
  shiny::fluidRow(
    purrr::pmap(
      specialties,
      \(code, specialty) {
        shiny::tagList(
          col_6(glue::glue("{specialty} ({code})")),
          col_6(
            shiny::selectInput(
              ns(glue::glue("specialty_{code}")),
              NULL,
              "Other",
              width = "100%"
            )
          )
        )
      }
    )
  )
}
