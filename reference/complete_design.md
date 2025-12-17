# Complete design with coded responses

Complete design with coded responses

## Usage

``` r
complete_design(
  coded,
  units,
  design,
  identifiers = c("group_id", "login_name", "login_code"),
  overwrite = FALSE,
  missings = NULL
)
```

## Arguments

- coded:

  Tibble. Response data coded with
  [`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md).
  The argument `prepare` must be `TRUE`.

- units:

  Tibble. Unit data retrieved from the IQB Studio after setting the
  argument `unit_definition = TRUE` for
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
  – otherwise the page order of variables could not be correctly
  inferred from the variable source tree. Could already be treated by
  [`add_coding_scheme()`](https://iqb-research.github.io/eatPrepTBA/reference/add_coding_scheme.md)
  to save some time.

- design:

  Tibble. Design retrieved from Testcenter via
  [`get_design()`](https://iqb-research.github.io/eatPrepTBA/reference/get_design.md)
  or an object formatted in the same way.

- identifiers:

  Character. Contains person identifiers of the dataset `coded`.
  Defaults to `c("group_id", "login_name", "login_code")` which
  corresponds to the identifiers of the IQB Testcenter.

- overwrite:

  Logical. Should column `unit_codes` be overwritten if they exist on
  `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they
  were added to `units` beforehand by applying `add_coding_schemes()`.

- missings:

  Tibble (optional). Provide missing meta data with `code_id`, `status`,
  `score`, and `code_type`. Defaults to `NULL` and uses default scheme.
  (Currently, only one missing scheme is supported.)

  This function automatically completes missings for coded responses.

## Value

A tibble.
