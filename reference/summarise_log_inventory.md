# Summarise log event types

**\[experimental\]**

Counts log event types and classifies whether an event type is known
from Testcenter logging and whether eatPrepTBA currently has a parser
for it. This function is intended as a cheap first diagnostic before
parsing large log data in detail.

## Usage

``` r
summarise_log_inventory(logs)
```

## Arguments

- logs:

  Tibble. Logs retrieved with
  [`get_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/get_logs.md)
  or read with
  [`read_logs()`](https://iqb-research.github.io/eatPrepTBA/reference/read_logs.md).

## Value

A tibble with one row per log type.
