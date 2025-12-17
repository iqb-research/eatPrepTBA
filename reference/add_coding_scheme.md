# Adds prepared coding scheme to units

Returns the `units` object with added column `unit_codes`. The routine
can also propose variable pages if
[`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
was called with `unit_definition = TRUE`. Please note that no other
operation except for filtering or
[`add_metadata()`](https://iqb-research.github.io/eatPrepTBA/reference/add_metadata.md)
should be applied to the `units`.

## Usage

``` r
add_coding_scheme(units, filter_has_codes = TRUE, overwrite = FALSE)
```

## Arguments

- units:

  Tibble. Contains units retrieved from
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md).

- filter_has_codes:

  Logical. Only returns variables that were not deactivated. Defaults to
  `TRUE`.

- overwrite:

  Logical. Should potentially existing `unit_codes` be overwritten?
  Defaults to `FALSE`.

## Value

A tibble.
