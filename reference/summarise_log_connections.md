# Summarise connection state logs

**\[experimental\]**

Summarises `CONNECTION` events per test session. These values describe
Testcenter connection states and transitions; they are not
download-speed or bandwidth measurements.

## Usage

``` r
summarise_log_connections(logs, session_cols = NULL)
```

## Arguments

- logs:

  Tibble. Logs retrieved with
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or read with
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

- session_cols:

  Optional character vector with columns defining a test session. By
  default, all available session columns are used.

## Value

A tibble with one row per session.
