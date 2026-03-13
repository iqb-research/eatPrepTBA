# Estimates stay times from log data

**\[experimental\]**

Returns estimated stay times for units and unit pages.

## Usage

``` r
estimate_unit_times(logs)
```

## Arguments

- logs:

  Tibble. Must be a logs tibble retrieved with
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

## Value

Contains the estimated stay times of units and pages.
