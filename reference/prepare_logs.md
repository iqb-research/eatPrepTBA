# Prepares logs

**\[experimental\]**

This function returns the logs in a format where the values are still
list columns that need to be unpacked.

## Usage

``` r
prepare_logs(logs, log_events = NULL)
```

## Arguments

- logs:

  Tibble. Responses retrieved from the IQB Testcenter via
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or from an extracted csv and read via
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

- log_events:

  Character vector. Names of events to be filtered for.

## Value

A tibble.
