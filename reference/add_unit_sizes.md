# Add unit resource sizes to log-derived unit data

**\[experimental\]**

Joins unit resource sizes from
[`compute_sizes()`](https://iqb-research.github.io/eatPrepTBA/reference/compute_sizes.md)
to log-derived unit data. Unit names are matched both with and without a
trailing `.xml` extension. The function adds byte and MiB sizes and,
when load-time columns are present, observed load-time-per-MiB
indicators. These indicators are descriptive load-time ratios and should
not be interpreted as download speed.

## Usage

``` r
add_unit_sizes(
  unit_data,
  sizes,
  unit_col = NULL,
  size_name_col = "name",
  size_col = "total_size"
)
```

## Arguments

- unit_data:

  Tibble. Unit-level or unit-playback data, for example returned by
  [`estimate_unit_times()`](https://iqb-research.github.io/eatPrepTBA/reference/estimate_unit_times.md)
  or state-specific log summaries.

- sizes:

  Tibble. Output of
  [`compute_sizes()`](https://iqb-research.github.io/eatPrepTBA/reference/compute_sizes.md).

- unit_col:

  Optional name of the unit identifier column in `unit_data`. Defaults
  to `unit_key` when available, otherwise `unit_alias`.

- size_name_col:

  Name column in `sizes`. Defaults to `name`, matching
  [`compute_sizes()`](https://iqb-research.github.io/eatPrepTBA/reference/compute_sizes.md)
  output.

- size_col:

  Size column in `sizes`. Defaults to `total_size`, matching
  [`compute_sizes()`](https://iqb-research.github.io/eatPrepTBA/reference/compute_sizes.md)
  output.

## Value

A tibble.
