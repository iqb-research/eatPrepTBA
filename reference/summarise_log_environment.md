# Summarise LOADCOMPLETE environment logs

**\[experimental\]**

Extracts browser, operating system, device, screen size, orientation,
and initial `LOADCOMPLETE` load time. Multiple `LOADCOMPLETE` rows per
session are retained as diagnostics via count and conflict columns while
the first non-missing value per field is returned. Sessions without
`LOADCOMPLETE` are retained with zero event counts and missing
environment fields.

## Usage

``` r
summarise_log_environment(logs, session_cols = NULL)
```

## Arguments

- logs:

  Tibble. Logs retrieved with
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or read with
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

- session_cols:

  Optional character vector with columns defining a test session. By
  default, all available columns among `group_id`, `login_name`,
  `login_code`, and `booklet_id` are used.

## Value

A tibble with one row per session.
