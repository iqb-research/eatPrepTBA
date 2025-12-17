# Flexibly adds a full version of the variable dependency tree

Extends the given coding scheme of a unit by its dependency tree. Please
note that the tree only comprises variable `variable_ref`, but not
`variable_id`.

## Usage

``` r
prepare_source_tree(coding_scheme)
```

## Arguments

- coding_scheme:

  Coding scheme as prepared by
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md)
  with setting the argument `coding_scheme = TRUE`.

## Value

A tibble.
