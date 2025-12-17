# Prepares a readable version of a coding scheme of one unit

Extends the given coding scheme of a unit to a full data frame that can
be filtered for batch checks.

## Usage

``` r
prepare_coding_scheme(coding_scheme, filter_has_codes = TRUE)
```

## Arguments

- coding_scheme:

  Coding scheme as prepared by
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
  with setting the argument `coding_scheme = TRUE`.

- filter_has_codes:

  Only returns variables that were not deactivated. Defaults to `TRUE`.

## Value

A tibble.
