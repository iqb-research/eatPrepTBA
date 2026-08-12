# Summarise player state logs

**\[experimental\]**

Counts raw `PLAYER` state events per session/unit. This is a compact
state summary and does not replace
[`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md)
for duration estimates.

## Usage

``` r
summarise_log_player(logs, session_cols = NULL, unit_cols = NULL)
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
