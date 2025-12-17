# Code unit responses with coding schemes

Code unit responses with coding schemes

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

  Tibble. Response data retrieved from the IQB Testcenter with setting
  the argument `prepare = FALSE` for
  [`get_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/get_responses.md)
  or
  [`read_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/read_responses.md).

- units:

  Tibble. Unit data retrieved from the IQB Studio with
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md).

- prepare:

  Logical. Whether to unpack the coding results and to add information
  from the coding schemes.

- overwrite:

  Logical. Should column `unit_codes` be overwritten if they exist on
  `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they
  were added to `units` beforehand by applying `add_coding_schemes()`.

- codes_manual:

  Tibble (optional). Data frame holding the manual codes. Defaults to
  `NULL` and does only automatic coding.

- missings:

  Tibble (optional). Provide missing meta data with `code_id`,
  `code_status`, `code_score`, and `code_type`. Defaults to `NULL` and
  uses default scheme.

  This function automatically codes responses by using the `eatAutoCode`
  package. It is already prepared for the new data format of the
  responses received from the
  [`get_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/get_responses.md)
  and
  [`read_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/read_responses.md)
  routines. This function will soon be deleted and be part of
  `code_responses()`.

## Value

A tibble.
