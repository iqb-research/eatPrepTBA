# Summarise focus state logs

**\[experimental\]**

Summarises raw `FOCUS` events per session. Durations are computed from
the first `FOCUS = HAS_NOT` in a focus-loss interval until the next raw
`FOCUS = HAS` event. Repeated `HAS_NOT` events before regain are counted
separately but do not shorten the interval.

## Usage

``` r
summarise_log_focus(logs, session_cols = NULL, focus_loss_threshold_ms = 5 * 60 * 1000)
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

- focus_loss_threshold_ms:

  Numeric. Focus losses longer than this threshold are counted as very
  long focus losses.

## Value

A tibble with one row per session.
