
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iHealth

<!-- badges: start -->
<!-- badges: end -->

The goal of iHealth is to provide an easy API to work with data exported
from Apple Health.

## Installation

You can install the development version of iHealth like so:

``` r
remotes::install_github("overdodactyl/iHealth")
```

## Example

Read data exported from Apple Health into a data frame (note, this can
take a couple minutes):

``` r
library(iHealth)
apple_health <- ihealth_read("/path/to/apple_health_export/")
```

Summarize distance moved during a given time period:

``` r
distance <- ihealth_distance(
  apple_health,
  start = as.Date("2023-06-26"),
  end = Sys.Date()
)
```
