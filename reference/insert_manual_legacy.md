# Add manual codes to unit responses

This function automatically codes responses of one unit by using the
`eatAutoCode` package.

## Usage

``` r
insert_manual_legacy(unit_responses, unit_codes_manual)
```

## Arguments

- unit_responses:

  Character. Response data of one unit retrieved from the IQB Testcenter
  in JSON format.

- unit_codes_manual:

  List. Manual codes to insert into the response data.

## Value

A tibble.
