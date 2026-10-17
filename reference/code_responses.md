# Code unit responses with coding schemes

This function automatically codes responses by using the `eatAutoCode`
package. It is prepared for the response format returned by
[`get_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/get_responses.md)
and
[`read_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/read_responses.md).
Rows with `responses = NA` are removed before coding schemes are
prepared and are not sent to the auto-coder; they represent units with
no stored response payload and are left for
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
to classify against the full test design.

## Usage

``` r
code_responses(
  responses,
  units,
  prepare = FALSE,
  overwrite = FALSE,
  codes_manual = NULL,
  missings = NULL
)
```

## Arguments

- responses:

  Data frame. Response data retrieved from the IQB Testcenter with
  setting the argument `prepare = FALSE` for
  [`get_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/get_responses.md)
  or
  [`read_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/read_responses.md).

- units:

  Data frame. Unit data retrieved from the IQB Studio with
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md).

- prepare:

  Logical. Whether to unpack the coding results and to add information
  from the coding schemes.

- overwrite:

  Logical. Should column `unit_codes` be overwritten if they exist on
  `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they
  were added to `units` beforehand by applying `add_coding_schemes()`.

- codes_manual:

  Data frame (optional). Data frame holding the manual codes. Defaults
  to `NULL` and does only automatic coding.

- missings:

  Data frame (optional). Provide missing meta data with `code_id`,
  `code_status`, `code_score`, and `code_type`. Defaults to `NULL` and
  uses default scheme.

## Value

A tibble, including logical `coding_from_not_reached`.

## Details

The logical column `coding_from_not_reached` records whether a response
was supplied as `NOT_REACHED` and its own coding rule treated that
status as a changed value (`TAKE_NOT_REACHED_AS_VALUE_CHANGED`). It is
captured before coding, independently of whether `value` is missing.
Manual codes that are applied take precedence: an effective non-NR
status override clears the marker even when its variable's processing
rule cannot be resolved. A manual NR converted by this function is
marked accordingly. Manual records without a status override retain the
original status and provenance; an explicit status override with unknown
provenance replaces the previous marker with `NA`. Unknown provenance is
`NA`; generated derived responses do not inherit the flag from their
sources. The flag is returned with either value of `prepare`.
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
uses confirmed NR provenance only for its not-reached boundary; the
response's analytical code and technical status are retained.
