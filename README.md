
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{nflAnalysis}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/TylerPollard410/nflAnalysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/TylerPollard410/nflAnalysis?branch=main)
<!-- badges: end -->

## Installation

You can install the development version of `{nflAnalysis}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
nflAnalysis::run_app()
```

## About

You are reading the doc about version : 0.0.1

This README has been compiled on the

``` r
Sys.time()
#> [1] "2024-11-13 21:29:48 EST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> Writing 'NAMESPACE'
#> ℹ Loading nflAnalysis
#> ── R CMD check results ────────────────────────────────── nflAnalysis 0.0.1 ────
#> Duration: 32.1s
#> 
#> ❯ checking DESCRIPTION meta-information ... NOTE
#>   Malformed Description field: should contain one or more complete sentences.
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> nflAnalysis Coverage: 94.42%
#> R/run_app.R: 0.00%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
#> R/golem_utils_server.R: 100.00%
#> R/golem_utils_ui.R: 100.00%
#> R/mod_name_of_module1.R: 100.00%
#> R/mod_name_of_module2.R: 100.00%
```
