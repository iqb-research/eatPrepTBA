# Evaluates frequencies and discrimination parameters of codes and categories

Evaluates frequencies and discrimination parameters of codes and
categories

## Usage

``` r
evaluate_psychometrics(
  design_coded,
  units,
  domains = NULL,
  max_n_categories = 10,
  overwrite = FALSE,
  identifiers = c("group_id", "login_name", "login_code")
)
```

## Arguments

- design_coded:

  Tibble. Response data within the design merged by
  [`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md).

- units:

  Tibble. Unit data retrieved from the IQB Studio after setting the
  argument `metadata = TRUE` for
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
  – otherwise the item values could only be inferred from the variable
  source tree, i.e., item scores are taken from variable scores that are
  no source variables for other derived variables. Could optionally also
  contain `unit_codes` prepared by
  [`add_coding_scheme()`](https://iqb-research.github.io/eatPrepTBA/reference/add_coding_scheme.md)
  (saves some time).

- domains:

  Tibble. Contains columns `domain` and `unit_key`. Currently, the
  routine only works for one-dimensional `domain`, i.e., there is only
  one `domain` for each `unit_key`. If not specified, the
  `workspace_label` is regarded as the unit domain.

- max_n_categories:

  Tibble. Maximum number of categories to check for category frequencies
  for list values, e.g., `[[01_1,01_2]]`. Defaults to `10`.

- overwrite:

  Logical. Should column `unit_codes` be overwritten if they exist on
  `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they
  were added to `units` beforehand by applying `add_coding_schemes()`.

- identifiers:

  Character. Contains person identifiers of the dataset `coded`.
  Defaults to `c("group_id", "login_name", "login_code")` which
  corresponds to the identifiers of the IQB Testcenter.

## Value

A tibble.

## Details

This function estimates item, code and category frequencies for a set of
coded responses. Please note that cases that are not used will be
eliminated prior to the anaylsis.
