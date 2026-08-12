# Summarise response and presentation progress logs

**\[experimental\]**

Summarises raw `RESPONSE_PROGRESS` and `PRESENTATION_PROGRESS` events
per session/unit.

## Usage

``` r
summarise_log_progress(logs, session_cols = NULL, unit_cols = NULL)
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

- unit_cols:

  Optional character vector with columns defining units. By default,
  available columns among `unit_key` and `unit_alias` are used.

## Value

A tibble with one row per session/unit.
