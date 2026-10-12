# Summarise item-level missings per person

Counts the final missing classifications on item level and divides each
count by the number of items assigned to that person in the completed
design. Uses `code_type`; neither scores nor technical statuses are used
to infer or change classifications.

## Usage

``` r
summarise_missings(
  data,
  identifiers = c("group_id", "login_name", "login_code"),
  used_only = FALSE
)
```

## Arguments

- data:

  Data frame returned by
  [`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md)
  and enriched with
  [`add_item_id()`](https://iqb-research.github.io/eatPrepTBA/reference/add_item_id.md).
  Requires `unit_key`, `item_id`, `code_type`, and all `identifiers`.
  Unmapped variables have a missing or blank `item_id`.

- identifiers:

  Character vector of columns that jointly identify a person. All
  specified columns must exist and contain non-missing, non-blank
  values. Include an occasion identifier when summarising separate
  administrations of the same items.

- used_only:

  Logical. Keep only people with `id_used = TRUE`? Defaults to `FALSE`,
  retaining all people in `data`. If present, `id_used` must be logical,
  non-missing, and constant within each person; it is returned in the
  result. It is required when `used_only = TRUE`.

## Value

An ungrouped tibble (also a data frame) with one row per person, the
identifiers, optional `id_used`, and `n_items`. Counts have prefix `n_`;
proportions have prefix `prop_` and range from 0 to 1:

- `missing`: total of all `MISSING_*` types except `MISSING_BY_DESIGN`.

- `not_reached`, `by_omission`, `invalid_response`, `coding_impossible`:
  the four standard missing types, always included even when absent.

- Additional observed missing types use their full type as a suffix,
  converted to lowercase and made syntactically valid with
  [`make.names()`](https://rdrr.io/r/base/make.names.html), e.g.
  `MISSING_TIMEOUT` gives `n_missing_timeout` and
  `prop_missing_timeout`. Ambiguous generated names are an error.

- `no_coding`, `coding_incomplete`, `derive_pending`,
  `intended_incomplete`: unresolved coding states, always included and
  excluded from the `missing` total.

- `unclassified`: missing or blank `code_type`, also excluded from the
  `missing` total. Other non-missing types contribute to `n_items` only.

## Details

An item is identified by `unit_key` and `item_id` together. Variables
without an item mapping are excluded from item counts. More than one row
for the same person and item is an error, even if the rows are
identical. Resolve duplicate mappings or repeated administrations before
summarising.

The denominator `n_items` includes all mapped items in the person's
design, including missing responses and unresolved coding results.
Combinations absent from the design are not added. Explicit
`MISSING_BY_DESIGN` rows are excluded from both the denominator and
missing counts. Retain all design rows before calling this function;
response-only data underestimate the denominator and missings.

All people in the input are retained by default, including those without
mapped items. People without eligible items receive zero counts and
`NA_real_` proportions. Empty input returns zero rows with the standard
result columns. Person order follows first appearance in the input.
Existing grouping is ignored.

## See also

[`complete_design()`](https://iqb-research.github.io/eatPrepTBA/reference/complete_design.md),
[`add_item_id()`](https://iqb-research.github.io/eatPrepTBA/reference/add_item_id.md)

## Examples

``` r
data <- tibble::tibble(
  person = c("P1", "P1", "P1", "P2"),
  unit_key = "U1", item_id = c("01", "02", NA, "01"),
  code_type = c("FULL_CREDIT", "MISSING_NOT_REACHED",
                "MISSING_NOT_REACHED", "MISSING_BY_OMISSION"),
  id_used = TRUE
)
summarise_missings(data, identifiers = "person")
#> # A tibble: 2 × 23
#>   person id_used n_items n_missing prop_missing n_not_reached prop_not_reached
#>   <chr>  <lgl>     <int>     <int>        <dbl>         <int>            <dbl>
#> 1 P1     TRUE          2         1          0.5             1              0.5
#> 2 P2     TRUE          1         1          1               0              0  
#> # ℹ 16 more variables: n_by_omission <int>, prop_by_omission <dbl>,
#> #   n_invalid_response <int>, prop_invalid_response <dbl>,
#> #   n_coding_impossible <int>, prop_coding_impossible <dbl>, n_no_coding <int>,
#> #   prop_no_coding <dbl>, n_coding_incomplete <int>,
#> #   prop_coding_incomplete <dbl>, n_derive_pending <int>,
#> #   prop_derive_pending <dbl>, n_intended_incomplete <int>,
#> #   prop_intended_incomplete <dbl>, n_unclassified <int>, …
```
