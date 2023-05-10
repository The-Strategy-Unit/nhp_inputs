
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NHP Inputs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/The-Strategy-Unit/nhp_inputs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/The-Strategy-Unit/nhp_inputs/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This is a shiny application which controls to input parameters used to
run the NHP model.

## Extracting Data

The data extraction pipeline is built and maintained using the
[`{targets}`](https://books.ropensci.org/targets/) package.

In order to run it, first configure access to the database by creating a
`.Renviron` file. Inside of this, configure a variable `DB_SERVER` with
the name of the server that contains the data.

Once you have done this you can run `targets::tar_make()`. Note, this
can take 2-3 hours to run.

## Deployment

Deployment is controlled by GitHub Actions: \* On pushes to the `main`
branch the app is redeployed to `nhp_dev/inputs` \* On tagged releases
it is redeployed to `nhp/inputs`.
