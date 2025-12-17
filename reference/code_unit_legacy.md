# Code the responses of one unit responses with its coding scheme

This function automatically codes responses of one unit by using the
`eatAutoCode` package.

## Usage

``` r
code_unit_legacy(unit_responses, coding_scheme)
```

## Arguments

- unit_responses:

  Character. Response data of one unit retrieved from the IQB Testcenter
  in JSON format.

- coding_scheme:

  Character. Coding scheme of the unit retrieved from the IQB Studio
  after setting the argument `coding_scheme = TRUE` for
  [`get_units()`](https://iqb-research.github.io/eatPrepTBA/reference/get_units.md).

## Value

A tibble.
