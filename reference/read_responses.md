# Reads responses files

This function reads response files downloaded from the IQB Testcenter
and prepares them for further processing with
[`code_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/code_responses.md).
Rows with empty response payloads are kept as rows with `responses = NA`
so that the observed unit structure remains available; final missing
codes are assigned later by
[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
when the coded data are checked against the full test design. The raw
nested response slot ids are checked for missing required slots and
unexpected new slots, but the returned data are not changed by these
diagnostics.

## Usage

``` r
read_responses(files, diagnostics = c("compact", "full", "none"))
```

## Arguments

- files:

  Character. Vector of paths to the csv files from the IQB Testcenter to
  be read.

- diagnostics:

  Character. Controls response slot diagnostics. Use `"compact"` for
  concise feedback, `"full"` for all details, or `"none"` to suppress
  these diagnostics.

## Value

A tibble.
