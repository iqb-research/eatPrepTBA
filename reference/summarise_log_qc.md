# Summarise log quality anomalies by session

**\[experimental\]**

Condenses the long anomaly table into one row per session, retaining
counts by severity and the set of anomaly codes observed. Sessions with
no detected anomalies are included.

## Usage

``` r
summarise_log_qc(logs, anomalies = NULL, session_cols = NULL, ...)
```

## Arguments

- logs:

  Tibble. Logs retrieved with
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or read with
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

- anomalies:

  Optional tibble returned by
  [`detect_log_anomalies()`](https://iqb-research.github.io/eatPrepTBA/reference/detect_log_anomalies.md).
  When omitted, anomalies are detected from `logs` using default
  settings.

- session_cols:

  Optional character vector with columns defining a test session. By
  default, all available columns among `group_id`, `group`,
  `login_name`, `login`, `login_code`, and `booklet_id` are used.

- ...:

  Additional arguments passed to
  [`detect_log_anomalies()`](https://iqb-research.github.io/eatPrepTBA/reference/detect_log_anomalies.md)
  when `anomalies` is omitted.

## Value

A tibble with one row per session.
