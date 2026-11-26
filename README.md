# OpenPlan Hospital Demand Model: Inputs App

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/The-Strategy-Unit/nhp_inputs/actions/workflows/check.yaml/badge.svg)](https://github.com/The-Strategy-Unit/nhp_inputs/actions/workflows/check.yaml)
<!-- badges: end -->

## About

A web app to input the parameters needed to run scenarios through the OpenPlan hospital demand model.

The app is [deployed to Posit Connect](https://connect.strategyunitwm.nhs.uk/nhp/inputs/).
You must have an account and sufficient permissions to view it.

Results can then be viewed in [the outputs app](https://connect.strategyunitwm.nhs.uk/nhp/outputs/), which is generated from the [nhp_outputs](https://github.com/The-Strategy-Unit/nhp_outputs) repository.

You can find more information in the [OpenPlan hospital demand model documentation](https://connect.strategyunitwm.nhs.uk/nhp/project_information/), including [a diagram](https://connect.strategyunitwm.nhs.uk/nhp/project_information/project_plan_and_summary/components-overview.html) of how the components of the modelling process fit together.

## For developers

The guidance below is for the members of [The Strategy Unit's Data Science team](https://the-strategy-unit.github.io/data_science/), who built and maintain this app.

### Structure

Visitors to the inputs app will actually land on the inputs selection app first.
Its code is packaged in [a separate repo](https://github.com/The-Strategy-Unit/nhp_inputs_selection_app).
The selection app directs users to either the latest inputs app deployment (default), or a previous version (with sufficient permissions).

Both apps are built with [Shiny](https://shiny.posit.co/) and [the {golem} framework](https://thinkr-open.github.io/golem/).
Server and UI modules can be found in `R/`, configuration in `inst/golem-config.yml` and supporting data and text in `inst/app/`.

### Run locally

Run the app locally on your machine to test that your changes work as expected.

#### Setup

First, install the required packages listed in the `DESCRIPTION` with

```r
pak::local_install_dev_deps()
```

Then add an `.Renviron` file to the project root that contains the required environment variables.
Copy into it the required keys, which are listed in the `.Renviron.example`.
You can get the values you need from a member of the Data Science team.

#### Run the app

Once setup, you can run the app locally:

```r
source("dev/watch.R")
```

Making selections in the app will cause values to be written to a local json file, which will live in your local `params/[development]/` directory.
These scenarios will be selectable and editable in future from your locally-run inputs selection app.
They will not be available from the deployed app.

### Test in dev

During pre-release QA we test both apps on the server.
This helps us spot any issues that are server-specific and might be overlooked if running the apps locally.

To test the unreleased app in our development environment:

1. Merge any PRs you want to test into the `main` branch.
2. If you've changed any dependencies, run `dev/generate_manifest.R` to update `manifest.json`.
3. If you've made any changes to the inputs selection app, run the manual `deploy()` call under the 'development' heading in the `deploy.R` script in your selection-app development branch.

When using the dev inputs selection app on Connect, make sure to set the 'Model Version' dropdown to 'dev' in the expandable 'Advanced Options' box.
That way you'll be taken to the dev inputs app when you hit 'Start'.

### Data

The app displays trust-specific data to users.
The data is processed via Databricks scripts in [the nhp_data repository](https://github.com/The-Strategy-Unit/nhp_data) and stored in Azure storage.

If the data updates and you need to invalidate the current cache, authorised developers can force a reset by appending `?reset_cache=true` to the app's canonical URL at `https://connect.strategyunitwm.nhs.uk/nhp/inputs/`.

### Deployment

Deployment is controlled by GitHub Actions in `.github/workflows/`, where:

- pushes to the `main` branch redeploy the app to /nhp/dev/inputs/ for purposes of quality assurance
- tagged releases trigger a new deployment to /nhp/vX-Y/inputs/, where 'vX-Y' is the current version (note the hyphen)
- manual deployment is possible with `connect-publish-manual.yaml`
