# Prepare unpacked response codes

**\[experimental\]**

This helper converts unpacked code-bearing response records into the
same core column shape returned by
`code_responses(..., prepare = TRUE)`: `variable_id`, `value`,
`code_id`, `code_score`, `code_status`, and `code_type`. By default,
`value` is unnested so prepared rows can be bound with prepared
[`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md)
output before calling
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md).

## Usage

``` r
prepare_unpacked_codes(
  unpacked,
  response_id = "value",
  variable_id_from = c("subform", "response_name", "response_id"),
  keep_uncoded = FALSE,
  unnest_value = TRUE,
  code_type = NA_character_
)
```

## Arguments

- unpacked:

  Data frame. Output from
  [`unpack_response_jsons()`](https://iqb-research.github.io/eatPrepTBA/reference/unpack_response_jsons.md).

- response_id:

  Character vector. Parsed response identifiers to keep. Defaults to
  `"value"`, which is the code-bearing entry in BKT question slots.

- variable_id_from:

  Character. Source used to derive `variable_id`. Defaults to `subform`,
  with fallback to `response_name` and then `response_id`.

- keep_uncoded:

  Logical. Whether target response rows without `code_id` and
  `code_score`, and source rows without target response records, should
  be retained.

- unnest_value:

  Logical. Whether to unnest the `value` list-column for compatibility
  with `code_responses(..., prepare = TRUE)`.

- code_type:

  Character. Default `code_type` for prepared rows when the unpacked
  data do not already contain `code_type`.

## Value

A tibble.
