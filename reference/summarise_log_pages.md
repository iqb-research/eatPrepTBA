# Summarise page state logs

**\[experimental\]**

Summarises raw `CURRENT_PAGE_ID`, `CURRENT_PAGE_NR`, and `PAGE_COUNT`
events per session/unit. `reached_last_page_nr` only checks whether the
maximum observed page number reached the reported page count.
`observed_pages_complete` is stricter and is only `TRUE` when all
integer page numbers from 1 to `PAGE_COUNT` were observed and the page
count was consistent; otherwise it is `FALSE` for visible gaps or `NA`
when completeness cannot be judged.

## Usage

``` r
summarise_log_pages(logs, session_cols = NULL, unit_cols = NULL)
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
