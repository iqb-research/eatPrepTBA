# Prepares responses

**\[experimental\]**

This function returns the responses in a format where the values are
still list columns that need to be unpacked.

## Usage

``` r
prepare_responses(responses)
```

## Arguments

- responses:

  Tibble. Responses retrieved from the IQB Testcenter via
  [`get_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/get_responses.md)
  or from an extracted csv and read via
  [`read_responses()`](https://iqb-research.github.io/eatPrepTBA/reference/read_responses.md).

## Value

A tibble.
