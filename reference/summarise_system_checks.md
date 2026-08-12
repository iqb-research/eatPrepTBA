# Summarise system check data

**\[experimental\]**

Produces one row per system-check case and detects network-related
fields conservatively from either wide columns or long
`variable_id`/`value` pairs. Network metrics are retained as a list
column because system-check exports may vary while these files are
evolving.

## Usage

``` r
summarise_system_checks(system_checks, check_cols = NULL)
```

## Arguments

- system_checks:

  Tibble. Output of
  [`get_system_checks()`](https://iqb-research.github.io/eatPrepTBA/reference/get_system_checks.md)
  or
  [`read_system_checks()`](https://iqb-research.github.io/eatPrepTBA/reference/read_system_checks.md).

- check_cols:

  Optional character vector defining one system-check case. By default,
  available identifier columns such as `Name`, group/login columns,
  booklet, and unit columns are used.

## Value

A tibble.
