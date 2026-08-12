# Detect potentially unreliable log patterns

**\[experimental\]**

Returns a long evidence table with one row per detected log anomaly. The
function is intentionally conservative: it focuses on structural
reliability signals that can be derived from ordinary Testcenter logs,
such as malformed `LOADCOMPLETE` rows, player loading/running
inconsistencies, connection loss, unresolved focus loss, runtime errors,
timestamp problems, and inconsistent page counters.

## Usage

``` r
detect_log_anomalies(
  logs,
  session_cols = NULL,
  unit_cols = NULL,
  focus_loss_threshold_ms = 5 * 60 * 1000,
  connection_transition_threshold = 10L,
  include_unknown_events = FALSE
)
```

## Arguments

- logs:

  Tibble. Logs retrieved with
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or read with
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

- session_cols:

  Optional character vector with columns defining a test session. By
  default, all available columns among `group_id`, `group`,
  `login_name`, `login`, `login_code`, and `booklet_id` are used.

- unit_cols:

  Optional character vector with columns defining units. By default,
  available columns among `unit_key` and `unit_alias` are used.

- focus_loss_threshold_ms:

  Numeric. Focus losses longer than this threshold are flagged.

- connection_transition_threshold:

  Integer. Sessions with more connection state transitions than this
  threshold are flagged.

- include_unknown_events:

  Logical. Should event types unknown to eatPrepTBA be returned as
  informational anomalies? Defaults to `FALSE` because large log
  datasets may contain many player-specific entries.

## Value

A tibble with session identifiers, optional unit identifiers,
`anomaly_code`, `severity`, `ts_start`, `ts_end`, `n_events`,
`evidence`, and `message`.
