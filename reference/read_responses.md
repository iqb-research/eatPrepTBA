# Reads responses files

This function reads response files downloaded from the IQB Testcenter
and prepares them for further processing with
[`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md).
Rows with empty response payloads are kept as rows with `responses = NA`
so that the observed unit structure remains available; final missing
codes are assigned later by
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
when the coded data are checked against the full test design.

## Usage

``` r
read_responses(files)
```

## Arguments

- files:

  Character. Vector of paths to the csv files from the IQB Testcenter to
  be read.

## Value

A tibble.
