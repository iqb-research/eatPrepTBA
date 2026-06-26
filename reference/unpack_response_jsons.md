# Unpack response JSON columns

**\[experimental\]**

This function unpacks response JSON payloads that are distributed across
several columns, for example `question_0_content`, `sums_content`, or
`responses`. Matching timestamp columns are detected by replacing a
`_content` suffix with `_ts`, for example `question_0_content` is
matched to `question_0_ts`. This is particularly useful for BKT-like
question-slot preparation where coded responses are distributed across
`question_*_content` columns instead of one `coded` column.

## Usage

``` r
unpack_response_jsons(
  responses,
  response_cols = NULL,
  id_cols = NULL,
  keep_empty_payloads = FALSE,
  keep_empty_rows = TRUE,
  progress = TRUE
)
```

## Arguments

- responses:

  Data frame. Response data with one or more JSON columns.

- response_cols:

  Character vector. Columns to unpack. If `NULL`, columns are
  auto-detected by parsing non-empty JSON payloads that contain response
  record fields such as `id`, `status`, `value`, `subform`, `code`, and
  `score`.

- id_cols:

  Character vector. Columns to keep as identifiers. If `NULL`, all
  non-JSON and non-timestamp columns are kept.

- keep_empty_payloads:

  Logical. Whether missing, empty, and `[]` payloads should be kept as
  rows with missing response identifiers.

- keep_empty_rows:

  Logical. Whether source rows that do not produce any unpacked JSON
  records should be kept as one row with missing response fields.

- progress:

  Logical. Whether to show a progress bar while parsing JSON payloads.

## Value

A tibble with the identifier columns, response slot metadata, and the
parsed JSON fields.
