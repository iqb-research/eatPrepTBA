# Code unit responses with coding schemes

**\[deprecated\]**

This function automatically codes responses by using the `eatAutoCode`
package.

## Usage

``` r
code_responses_legacy(
  responses,
  units,
  prepare = FALSE,
  by = NULL,
  codes_manual = NULL,
  missings = NULL,
  parallel = FALSE,
  n_cores = NULL
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

  Tibble. Unit data retrieved from the IQB Studio after setting the
  argument `coding_scheme = TRUE` for
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md).

- prepare:

  Logical. Whether to unpack the coding results and to add information
  from the coding schemes.

- by:

  Character. Additional columns as subgroups for the coding (e.g., in
  case of duplicate unit data for a specific person that could emerge in
  offline settings).

- codes_manual:

  Tibble (optional). Data frame holding the manual codes. Defaults to
  `NULL` and does only automatic coding.

- missings:

  Tibble (optional). Provide missing meta data with `code_id`, `status`,
  `score`, and `code_type`. Defaults to `NULL` and uses default scheme.

- parallel:

  Logical. Should the coding be conducted on multiple cores?

- n_cores:

  Integer. Number of the cores to be used for coding (only relevant if
  `parallel = TRUE`). Will default to number of available system cores -
  1.

## Value

A tibble.
