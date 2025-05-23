
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NHP Inputs

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/The-Strategy-Unit/nhp_inputs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/The-Strategy-Unit/nhp_inputs/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## About

An app to input the parameters needed to run scenarios through the New
Hospital Programme (NHP) demand model.

The app is [deployed to Posit
Connect](https://connect.strategyunitwm.nhs.uk/nhp/inputs/). You must
have an account and sufficient permissions to view it.

You can find more information on [the NHP model project information
site](https://connect.strategyunitwm.nhs.uk/nhp/project_information/),
including [a
diagram](https://connect.strategyunitwm.nhs.uk/nhp/project_information/project_plan_and_summary/components-overview.html)
of how the components of the modelling process fit together.

## For developers

The app is built and maintained by members of the Strategy Unit’s Data
Science team.

### Structure

The app is built primarily with [Shiny](https://shiny.posit.co/) and
[the {golem} package](https://thinkr-open.github.io/golem/). Server and
UI modules can be found in `R/`, configuration in
`inst/golem-config.yml` and supporting data and text in `inst/app/`.

Packages used in the app are listed in `DESCRIPTION`, and can be
installed with `devtools::install_deps(dependencies = TRUE)`.

### Run locally

Add an `.Renviron` file to the project root that contains the required
environment variables. You can get these from a member of the Data
Science team.

The inputs app is on the `main` branch, but the selection app (where
users start or edit a scenario) is in the `inputs_selection_app` branch.
Use Git’s `worktree` function to add that branch to its own folder in
your development branch:

    git fetch origin inputs_selection_app
    git worktree add inputs_selection_app inputs_selection_app

To run the app from RStudio, start up the selection app by opening the
`dev/watch.R` script, go to the ‘Background Jobs’ tab of the console
pane and click the ‘Start Background Job’ button. In the ‘Run Script as
Background Job’ dialog box select the project root as the ‘Working
Directory’, then hit ‘Start’. When ready, the app will tell you to visit
`http://127.0.0.1:9081/` in your browser. Note that your selections in
the app remain local to you.

If you need to remove the selection-app folder at any point, you can do
that with:

    git worktree remove inputs_selection_app

### Data extraction

The app displays trust-specific data to users. The data is processed via
Databricks scripts in [the nhp_data
repository](https://github.com/The-Strategy-Unit/nhp_data) and stored in
Azure storage.

### Deployment

Deployment is controlled by GitHub Actions, where:

- pushes to the `main` branch redeploy the app to /nhp/dev/inputs/ for
  purposes of quality assurance
- tagged releases trigger a new deployment to /nhp/vX-Y/inputs/, where
  ‘vX-Y’ is the current version (note the hyphen)
