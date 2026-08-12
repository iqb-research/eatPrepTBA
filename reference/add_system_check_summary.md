# Add system check summaries to log-derived data

**\[experimental\]**

Joins system-check summaries onto log-derived session data. The function
only joins by explicit or clearly shared identifier columns and errors
when no join key can be found.

## Usage

``` r
add_system_check_summary(log_data, system_check_summary, by = NULL)
```

## Arguments

- log_data:

  Tibble. Log-derived session data, for example from
  [`summarise_log_qc()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_qc.md)
  or
  [`summarise_log_environment()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_log_environment.md).

- system_check_summary:

  Tibble. Output of
  [`summarise_system_checks()`](https://iqb-research.github.io/eatPrepTBA/reference/summarise_system_checks.md).

- by:

  Optional character vector of join columns. If omitted, the
  intersection of common session identifier columns is used.

## Value

A tibble.
