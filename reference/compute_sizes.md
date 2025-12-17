# Computes resource sizes for Testcenter instances

Computes resource sizes for Testcenter instances

## Usage

``` r
compute_sizes(data)
```

## Arguments

- data:

  Tibble. Must be a tibble retrieved with
  [`list_files()`](https://iqb-research.github.io/eatPrepTBA/reference/list_files.md)
  with `dependencies = TRUE`.

## Value

The object with estimated sizes of units, booklets, and testtakers file
and adds a column `total_size` (in Bytes). Note that this only works for
objects retrieved from Testcenter 15.6.0 and higher.
